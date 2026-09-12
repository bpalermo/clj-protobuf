(ns clj-protobuf.codec
  "The codec half of the generated-code contract: `set-field!` and `get-field`.

  Generated `->proto` fns call `set-field!` once per field against a fresh
  builder; `proto->` fns call `get-field` once per field feeding the record's
  positional constructor. Both take an opts map that is almost always nil.

  Semantics the whole design hangs on:
  - nil means absent, in both directions. A record has every key; protobuf has
    presence. `set-field!` of nil sets nothing; `get-field` of an unset
    explicit-presence field returns nil. Fields with IMPLICIT presence
    (editions) and proto3 no-label scalars have no absence to report, so
    `get-field` returns the value — default included — and never nil.
  - Nested message values arrive as records or plain maps; generated code never
    calls the nested `->proto`, so recursion happens here, through the handle's
    child handles, which carry the concrete-class-correct nested prototypes.
  - The proto field name is the authority. Kebab keys are derived; the reverse
    mapping does not exist (STYLE_LEGACY).

  opts (all optional):
    :naming  :kebab (default) | :proto  — keys used on the generic map path
    :enums   :keyword (default; exact proto value name, e.g. :GREETING_HELLO)
             | :number | :string        — how get-field represents enums;
             set-field! accepts keyword, string, number or EnumValueDescriptor
             regardless
    :bytes   :byte-array (default) | :byte-string"
  ;; The :require is load-bearing even though only the class is used: importing
  ;; a record class requires the namespace that defines it to have been loaded.
  (:require [clj-protobuf.impl.message :as message]
            [clj-protobuf.runtime])
  (:import [clj_protobuf.impl.compile CompiledField CompiledType]
           [clj_protobuf.runtime FieldHandle]
           [clojure.lang PersistentArrayMap]
           [com.google.protobuf
            ByteString
            Descriptors$Descriptor
            Descriptors$EnumDescriptor
            Descriptors$EnumValueDescriptor
            Descriptors$FieldDescriptor
            Message
            Message$Builder]
           [java.util ArrayList Arrays LinkedHashMap List]
           [java.util.function BiFunction Function]))

(set! *warn-on-reflection* true)

(defn- type-mismatch [^FieldHandle h value expected]
  (throw (ex-info (str "field " (.getFullName ^Descriptors$FieldDescriptor (.-fd h))
                       " expects " expected ", got "
                       (some-> value class (.getName)))
                  {:clj-protobuf/error :type-mismatch
                   :field (.getFullName ^Descriptors$FieldDescriptor (.-fd h))
                   :expected expected
                   :value value})))

(defn- enum-value
  ^Descriptors$EnumValueDescriptor [^FieldHandle h v]
  (let [^Descriptors$EnumDescriptor et (.-enum-type h)]
    (or (cond
          ;; Keywords and numbers hit the handle's tables: findValueByName is
          ;; a string concatenation plus a pool lookup per call.
          (keyword? v) (get (.-enum-by-kw h) v)
          (string? v)  (.findValueByName et ^String v)
          (number? v)  (or (get (.-enum-by-number h) (long v))
                           ;; Open enums (proto3, editions default) accept
                           ;; numbers no declared value names.
                           (.findValueByNumberCreatingIfUnknown et (int v)))
          (instance? Descriptors$EnumValueDescriptor v) v
          :else nil)
        (type-mismatch h v (str "a value of enum " (.getFullName et))))))

(declare set-field! proto-value)

(defn- map->message
  "Build a nested message from a record or plain map through the child handles."
  ^Message [^FieldHandle h m opts]
  (let [b (.newBuilderForType ^Message (.-nested-prototype h))
        ^objects children @(.-children h)
        n (alength children)
        proto-keys? (= :proto (:naming opts))]
    (loop [i 0]
      (when (< i n)
        (let [^FieldHandle ch (aget children i)]
          (set-field! b ch (get m (if proto-keys? (.-proto-key ch) (.-kebab-key ch))) opts)
          (recur (inc i)))))
    (.build b)))

(defn- message-value
  ^Message [^FieldHandle h v opts]
  (let [^Message nested (.-nested-prototype h)]
    (cond
      (instance? Message v)
      (let [^Message mv v]
        (cond
          ;; Right concrete class and type already — the common case when a
          ;; caller used the nested type's own ->proto. The class alone does
          ;; not say: every compiled (or dynamic) message shares one.
          (and (identical? (class mv) (class nested))
               (identical? (.getDescriptorForType mv) (.getDescriptorForType nested)))
          mv
          ;; Same message type, different concrete class (DynamicMessage into a
          ;; generated builder, or vice versa): rebuild field-by-field. Costs a
          ;; copy, preserves the bytes.
          (= (.getFullName (.getDescriptorForType mv))
             (.getFullName (.getDescriptorForType nested)))
          (-> (.newBuilderForType nested) (.mergeFrom mv) (.build))
          :else (type-mismatch h v (str "a " (.getFullName (.getDescriptorForType nested))))))

      (map? v) (map->message h v opts)
      :else (type-mismatch h v "a record, map, or Message"))))

(defn- slot-value
  "Coerce one Clojure value to the compiled codec's slot representation:
  proto-value's, except enums, which are numbers there."
  [^FieldHandle h v opts]
  (let [x (proto-value h v opts)]
    (if (identical? :enum (.-kind h))
      (Integer/valueOf (.getNumber ^Descriptors$EnumValueDescriptor x))
      x)))

(defn- proto-value
  "Coerce one Clojure value to what protobuf-java's reflection API expects."
  [^FieldHandle h v opts]
  (case (.-kind h)
    :int     (if (number? v) (Integer/valueOf (.intValue ^Number v)) (type-mismatch h v "a number"))
    :long    (if (number? v) (Long/valueOf (.longValue ^Number v)) (type-mismatch h v "a number"))
    :float   (if (number? v) (Float/valueOf (.floatValue ^Number v)) (type-mismatch h v "a number"))
    :double  (if (number? v) (Double/valueOf (.doubleValue ^Number v)) (type-mismatch h v "a number"))
    :boolean (if (boolean? v) v (type-mismatch h v "a boolean"))
    :string  (if (string? v) v (type-mismatch h v "a string"))
    :bytes   (cond
               (bytes? v) (ByteString/copyFrom ^bytes v)
               (instance? ByteString v) v
               :else (type-mismatch h v "a byte array or ByteString"))
    :enum    (enum-value h v)
    :message (message-value h v opts)))

;; Collections, hinted arm: one clearX then one addAllX / putAllX through the
;; typed invokers. Clear first because the bulk accessors append and merge,
;; where setField replaces — and this fn replaces.
;;
;; Collections, reflection arm: ONE setField carrying a List — protobuf-java
;; documents List as the repeated-field value type — rather than
;; addRepeatedField per element, which repeats the accessor lookup N times.
;;
;; Map entries reach the builder in the Clojure map's iteration order on both
;; arms (the reflection arm's list, the invoker arm's LinkedHashMap), and
;; protobuf-java serializes map entries in insertion order: that is what keeps
;; the two arms byte-identical. A HashMap here would change the bytes.
(defn- set-map! [^Message$Builder b ^FieldHandle h m opts]
  (when-not (map? m) (type-mismatch h m "a map"))
  (let [fd ^Descriptors$FieldDescriptor (.-fd h)
        ^FieldHandle kh (.-key-handle h)
        ^FieldHandle vh (.-val-handle h)]
    (cond
      ;; compiled arm: the slot IS a LinkedHashMap in slot representation
      (and (message/compiled-builder? b) (.-slot h))
      (let [out (LinkedHashMap. (int (count m)))]
        (reduce-kv (fn [_ k v] (.put out (slot-value kh k opts) (slot-value vh v opts)))
                   nil m)
        (message/set-slot! b (.-slot h) out))

      (.-set-invoker h)
      (let [inv (.-set-invoker h)
            out (LinkedHashMap. (int (count m)))]
        (reduce-kv (fn [_ k v] (.put out (proto-value kh k opts) (proto-value vh v opts)))
                   nil m)
        (.apply ^Function (.-clear-invoker h) b)
        (.apply ^BiFunction inv b out))

      :else
      (let [^Message entry-proto (.-nested-prototype h)
            out (java.util.ArrayList. (count m))]
        (reduce-kv (fn [_ k v]
                     (let [eb (.newBuilderForType entry-proto)]
                       (.setField eb (.-fd kh) (proto-value kh k opts))
                       (.setField eb (.-fd vh) (proto-value vh v opts))
                       (.add out (.build eb))))
                   nil m)
        (.setField b fd out)))))

(defn- set-repeated! [^Message$Builder b ^FieldHandle h vs opts]
  (when-not (sequential? vs) (type-mismatch h vs "a sequential collection"))
  (let [out (java.util.ArrayList. (count vs))]
    (cond
      (and (message/compiled-builder? b) (.-slot h))
      (do (reduce (fn [_ v] (.add out (slot-value h v opts))) nil vs)
          (message/set-slot! b (.-slot h) out))

      (.-set-invoker h)
      (do (reduce (fn [_ v] (.add out (proto-value h v opts))) nil vs)
          (.apply ^Function (.-clear-invoker h) b)
          (.apply ^BiFunction (.-set-invoker h) b out))

      :else
      (do (reduce (fn [_ v] (.add out (proto-value h v opts))) nil vs)
          (.setField b ^Descriptors$FieldDescriptor (.-fd h) out)))))

(defn set-field!
  "Set one field on a builder from a Clojure value. nil sets nothing — that is
  how a record (all keys always present) maps onto protobuf presence. Mutates
  and returns the builder."
  ([builder handle v] (set-field! builder handle v nil))
  ([builder ^FieldHandle handle v opts]
   (when (some? v)
     (let [^Message$Builder b builder]
       (cond
         (.-map? handle)      (set-map! b handle v opts)
         (.-repeated? handle) (set-repeated! b handle v opts)
         ;; compiled arm: coerce and store the slot. A handle from another
         ;; arm has no slot and takes the reflective path, which the
         ;; compiled builder also speaks.
         (and (message/compiled-builder? b) (.-slot handle))
         (message/set-slot! b (.-slot handle) (slot-value handle v opts))

         :else
         (let [v' (proto-value handle v opts)]
           ;; The invoker replaces ONLY the accessor call: same converted
           ;; value, same builder, protobuf-java's FieldAccessorTable lookup
           ;; skipped. Absent (DynamicMessage arm, underivable accessor,
           ;; native-image) the reflection API is the path, as ever. An enum
           ;; invoker is setXValue(int), so it takes the number.
           (if-let [inv (.-set-invoker handle)]
             (.apply ^BiFunction inv b
                     (if (identical? :enum (.-kind handle))
                       (Integer/valueOf (.getNumber ^Descriptors$EnumValueDescriptor v'))
                       v'))
             (.setField b ^Descriptors$FieldDescriptor (.-fd handle) v'))))))
   builder))

(declare get-field)

(defn- enum-of-number
  "An enum number — from a getXValue invoker or a compiled slot — to its
  descriptor, which for an open enum may name no declared value."
  ^Descriptors$EnumValueDescriptor [^FieldHandle h ^long n]
  (or (get (.-enum-by-number h) n)
      (.findValueByNumberCreatingIfUnknown ^Descriptors$EnumDescriptor (.-enum-type h) (int n))))

(defn- message->map
  "A parsed nested message as a plain map. Only present fields appear; the
  consumer sees nil for the rest either way.

  Present fields are gathered into one key/value array and the map is built
  from it in a single step — an array map up to eight entries, a hash map
  beyond — rather than assoc'd one field at a time, which copies the map on
  every step. Measured at half the decode time of the reduce/assoc shape on
  lists of small messages."
  [^FieldHandle h ^Message m opts]
  (let [^objects children @(.-children h)
        n (alength children)
        arr (object-array (* 2 n))
        proto-keys? (= :proto (:naming opts))]
    (loop [i 0 j 0]
      (if (< i n)
        (let [^FieldHandle ch (aget children i)
              v (get-field m ch opts)]
          (if (some? v)
            (do (aset arr j (if proto-keys? (.-proto-key ch) (.-kebab-key ch)))
                (aset arr (inc j) v)
                (recur (inc i) (+ j 2)))
            (recur (inc i) j)))
        (cond
          (zero? j) {}
          ;; PersistentArrayMap's own promotion threshold: 8 entries.
          (> j 16) (loop [k 0 tm (transient {})]
                     (if (< k j)
                       (recur (+ k 2) (assoc! tm (aget arr k) (aget arr (inc k))))
                       (persistent! tm)))
          (= j (alength arr)) (PersistentArrayMap. arr)
          :else (PersistentArrayMap. (Arrays/copyOf arr j)))))))

(defn- clj-value
  [^FieldHandle h v opts]
  (case (.-kind h)
    (:int :long :float :double :boolean :string) v
    :bytes   (if (= :byte-string (:bytes opts))
               v
               (.toByteArray ^ByteString v))
    ;; An enum arrives as an EnumValueDescriptor from the reflection API or
    ;; as its number from a compiled slot or a getXValue invoker.
    :enum    (if (instance? Descriptors$EnumValueDescriptor v)
               (case (:enums opts :keyword)
                 :keyword (or (get (.-enum-kw h) v)
                              ;; open-enum unknowns are created on the fly and
                              ;; cannot be in the table
                              (keyword (.getName ^Descriptors$EnumValueDescriptor v)))
                 :number  (.getNumber ^Descriptors$EnumValueDescriptor v)
                 :string  (.getName ^Descriptors$EnumValueDescriptor v))
               (let [n (long v)]
                 (case (:enums opts :keyword)
                   :keyword (or (get (.-enum-kw-by-number h) n)
                                (keyword (.getName ^Descriptors$EnumValueDescriptor (enum-of-number h n))))
                   :number  n
                   :string  (.getName ^Descriptors$EnumValueDescriptor (enum-of-number h n)))))
    :message (message->map h v opts)))

(defn get-field
  "Read one field from a message as a Clojure value. nil means absent: an unset
  explicit-presence field, or an empty repeated/map field. IMPLICIT-presence
  fields (and proto3 no-label scalars) have no absence and return their value,
  default included. Nested messages come back as plain maps.

  The handle must be one built on this message's own prototype. A handle
  carries a FieldDescriptor from its prototype's descriptor pool, and on the
  hinted arm invokers over that concrete class's accessors, so reading a
  message of another arm throws rather than answering. See the pools section
  of docs/design.md."
  ([msg handle] (get-field msg handle nil))
  ([msg ^FieldHandle handle opts]
   (let [^Message m msg
         fd ^Descriptors$FieldDescriptor (.-fd handle)
         compiled (and (message/compiled-message? m) (some? (.-slot handle)))]
     (cond
       (.-map? handle)
       (let [^FieldHandle kh (.-key-handle handle)
             ^FieldHandle vh (.-val-handle handle)]
         (if (or compiled (.-get-invoker handle))
           ;; the slot's own map, or getXMap: no entry messages materialized
           (let [^java.util.Map jm (if compiled
                                     (message/message-slot m (.-slot handle))
                                     (.apply ^Function (.-get-invoker handle) m))]
             (when (and jm (pos? (.size jm)))
               (let [it (.iterator (.entrySet jm))]
                 (loop [acc (transient {})]
                   (if (.hasNext it)
                     (let [^java.util.Map$Entry e (.next it)]
                       (recur (assoc! acc
                                      (clj-value kh (.getKey e) opts)
                                      (clj-value vh (.getValue e) opts))))
                     (persistent! acc))))))
           (let [entries ^java.util.List (.getField m fd)]
             (when (pos? (.size entries))
               (persistent!
                (reduce (fn [acc ^Message e]
                          (assoc! acc
                                  (clj-value kh (.getField e (.-fd kh)) opts)
                                  (clj-value vh (.getField e (.-fd vh)) opts)))
                        (transient {})
                        entries))))))

       (.-repeated? handle)
       (let [^java.util.List vs (cond
                                  compiled (message/message-slot m (.-slot handle))
                                  (.-get-invoker handle) (.apply ^Function (.-get-invoker handle) m)
                                  :else (.getField m fd))]
         (when (and vs (pos? (.size vs)))
           (case (.-kind handle)
             ;; Scalars come back as they are: no per-element conversion.
             (:int :long :float :double :boolean :string) (vec vs)
             (persistent!
              (reduce (fn [acc v] (conj! acc (clj-value handle v opts)))
                      (transient [])
                      vs)))))

       compiled
       ;; nil is absent — or, without presence, the default, read back
       (let [v (message/message-slot m (.-slot handle))]
         (if (nil? v)
           (when-not (.-has-presence? handle)
             (when-some [d (.-slot-default handle)]
               (clj-value handle d opts)))
           (clj-value handle v opts)))

       :else
       (let [absent? (and (.-has-presence? handle)
                          (if-let [hs (.-has-invoker handle)]
                            (not (.apply ^Function hs m))
                            (not (.hasField m fd))))]
         (when-not absent?
           (clj-value handle
                      (if-let [g (.-get-invoker handle)]
                        (.apply ^Function g m)
                        (.getField m fd))
                      opts)))))))

;; ---------------------------------------------------------------------------
;; The typed write path's surface (0.4.0)
;;
;; The mirror of rt/slot, and deliberately not symmetric with it. A read could
;; hand back the slot as it stood, because a slot already holds the Clojure
;; value; a write cannot, because a record's :n may be a Long where an int32
;; slot must hold an Integer. So the COERCION LIVES HERE rather than in
;; emitted code, and that is the whole design:
;;
;; - a coercion baked into checked-in generated files is a contract nobody can
;;   change afterwards; behind this symbol it is a patch release.
;; - the elision rule for a field without presence compares the slot's default
;;   with .equals, and Integer(0).equals(Long(0)) is FALSE — so an uncoerced
;;   Clojure 0 would be stored live and serialize a field protobuf says must
;;   not appear. A byte difference, which only //test:byte_identity_test would
;;   have caught.
;; - nil clears a oneof's siblings, so emitted code writing a oneof's members
;;   in declaration order with a nil among them would clear the member it just
;;   set. This ignores nil instead, so every generated file cannot get that
;;   wrong rather than each one having to get it right.
;;
;; Emitted code therefore says WHICH field and nothing about how the
;; representation works.

(defn- slot-mismatch [^CompiledField f v expected]
  (let [fd ^Descriptors$FieldDescriptor (.-fd f)]
    (throw (ex-info (str "field " (.getFullName fd) " expects " expected ", got "
                         (some-> v class (.getName)))
                    {:clj-protobuf/error :type-mismatch
                     :field (.getFullName fd)
                     :expected expected
                     :value v}))))

(defn- slot-enum
  "A Clojure enum value to its number. The keyword table is precomputed per
  field; the other shapes stay accepted because set-field! accepts them."
  [^CompiledField f v]
  (or (cond
        (keyword? v) (get (.-enum-numbers f) v)
        (number? v) (Integer/valueOf (.intValue ^Number v))
        (instance? Descriptors$EnumValueDescriptor v)
        (Integer/valueOf (.getNumber ^Descriptors$EnumValueDescriptor v))
        (string? v) (when-let [evd ^Descriptors$EnumValueDescriptor
                               (.findValueByName ^Descriptors$EnumDescriptor (.-enum-type f)
                                                 ^String v)]
                      (Integer/valueOf (.getNumber evd)))
        :else nil)
      (slot-mismatch f v (str "a value of enum "
                              (.getFullName ^Descriptors$EnumDescriptor (.-enum-type f))))))

(defn- slot-message
  "A nested message for a slot, verified and repaired rather than trusted.

  Emitted code calls the nested X->proto, which builds through that type's OWN
  prototype — resolved independently of this one — so with generated classes
  present for one file and absent for another it can hand back a generated
  Java message where a slot must hold a compiled one. The emitter cannot
  promise otherwise, so this refuses to store a wrong one: identical descriptor
  on a compiled message is the fast path, the same type in another
  representation is rebuilt, and anything else is an error naming the field.
  Exactly what message-value does for set-field!, on the same grounds."
  [^CompiledField f v]
  (let [^CompiledType nt (deref (.-nested f))
        ^Descriptors$Descriptor d (.-descriptor nt)]
    (cond
      (and (message/compiled-message? v)
           (identical? (.getDescriptorForType ^Message v) d))
      v

      (and (instance? Message v)
           (= (.getFullName ^Descriptors$Descriptor (.getDescriptorForType ^Message v))
              (.getFullName d)))
      (-> (.newBuilderForType ^Message (message/prototype d))
          (.mergeFrom ^Message v)
          (.build))

      :else (slot-mismatch f v (str "a " (.getFullName d))))))

(defn- slot-scalar
  "One value in slot representation, for a field of this kind."
  [^CompiledField f kind v]
  (case kind
    :int     (if (number? v) (Integer/valueOf (.intValue ^Number v)) (slot-mismatch f v "a number"))
    :long    (if (number? v) (Long/valueOf (.longValue ^Number v)) (slot-mismatch f v "a number"))
    :float   (if (number? v) (Float/valueOf (.floatValue ^Number v)) (slot-mismatch f v "a number"))
    :double  (if (number? v) (Double/valueOf (.doubleValue ^Number v)) (slot-mismatch f v "a number"))
    :boolean (if (boolean? v) v (slot-mismatch f v "a boolean"))
    :string  (if (string? v) v (slot-mismatch f v "a string"))
    :bytes   (cond
               (bytes? v) (ByteString/copyFrom ^bytes v)
               (instance? ByteString v) v
               :else (slot-mismatch f v "a byte array or ByteString"))
    :enum    (slot-enum f v)
    :message (slot-message f v)))

(defn slot-set!
  "Coerce a Clojure value and store it in a compiled builder's slot, named by
  the field's declaration index — `(.getIndex fd)`, the same index rt/slot
  reads and rt/slot-of reports, which a generator bakes as a literal.

  nil is ignored, so emitted code is one unguarded call per field. That is not
  a convenience: writing nil would clear a oneof's siblings, so a generated
  file writing a oneof's members in order would clear the one it had just set.

  Default elision for fields without presence and oneof clearing are NOT done
  here — set-slot! already does both, correctly, and a second implementation
  is how they drift apart."
  [builder ^long i v]
  (when (some? v)
    (let [^CompiledField f (message/builder-field builder i)]
      (message/set-slot!
       builder i
       (cond
         (.-repeated? f)
         (if (sequential? v)
           (let [out (ArrayList. (count v))
                 kind (.-kind f)]
             (reduce (fn [_ x] (.add out (slot-scalar f kind x))) nil v)
             out)
           (slot-mismatch f v "a sequential collection"))

         (.-map? f)
         (if (map? v)
           (let [out (LinkedHashMap. (int (count v)))
                 kk (.-key-kind f)
                 vk (.-val-kind f)]
             (reduce-kv (fn [_ k x]
                          (.put out
                                (slot-scalar f kk k)
                                (if (identical? :enum vk)
                                  (or (get (.-val-enum-numbers f) x)
                                      (slot-scalar f vk x))
                                  (slot-scalar f vk x))))
                        nil v)
             out)
           (slot-mismatch f v "a map"))

         :else (slot-scalar f (.-kind f) v)))))
  builder)
