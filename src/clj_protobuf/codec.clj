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
  (:require [clj-protobuf.runtime])
  (:import [clj_protobuf.runtime FieldHandle]
           [clojure.lang PersistentArrayMap]
           [com.google.protobuf
            ByteString
            Descriptors$EnumDescriptor
            Descriptors$EnumValueDescriptor
            Descriptors$FieldDescriptor
            Message
            Message$Builder]
           [java.util Arrays LinkedHashMap]
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

(declare set-field!)

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
          ;; Right concrete class already — the common case when a caller used
          ;; the nested type's own ->proto.
          (identical? (class mv) (class nested)) mv
          ;; Same message type, different concrete class (DynamicMessage into a
          ;; generated builder, or vice versa): rebuild field-by-field. Costs a
          ;; copy, preserves the bytes.
          (= (.getFullName (.getDescriptorForType mv))
             (.getFullName (.getDescriptorForType nested)))
          (-> (.newBuilderForType nested) (.mergeFrom mv) (.build))
          :else (type-mismatch h v (str "a " (.getFullName (.getDescriptorForType nested))))))

      (map? v) (map->message h v opts)
      :else (type-mismatch h v "a record, map, or Message"))))

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
    (if-let [inv (.-set-invoker h)]
      (let [out (LinkedHashMap. (int (count m)))]
        (reduce-kv (fn [_ k v] (.put out (proto-value kh k opts) (proto-value vh v opts)))
                   nil m)
        (.apply ^Function (.-clear-invoker h) b)
        (.apply ^BiFunction inv b out))
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
    (reduce (fn [_ v] (.add out (proto-value h v opts))) nil vs)
    (if-let [inv (.-set-invoker h)]
      (do (.apply ^Function (.-clear-invoker h) b)
          (.apply ^BiFunction inv b out))
      (.setField b ^Descriptors$FieldDescriptor (.-fd h) out))))

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
    :enum    (case (:enums opts :keyword)
               :keyword (or (get (.-enum-kw h) v)
                            ;; open-enum unknowns are created on the fly and
                            ;; cannot be in the table
                            (keyword (.getName ^Descriptors$EnumValueDescriptor v)))
               :number  (.getNumber ^Descriptors$EnumValueDescriptor v)
               :string  (.getName ^Descriptors$EnumValueDescriptor v))
    :message (message->map h v opts)))

(defn- enum-of-number
  "An enum getter invoker is getXValue(): the number, which for an open enum
  may name no declared value."
  ^Descriptors$EnumValueDescriptor [^FieldHandle h ^Integer n]
  (or (get (.-enum-by-number h) (long n))
      (.findValueByNumberCreatingIfUnknown ^Descriptors$EnumDescriptor (.-enum-type h) (int n))))

(defn get-field
  "Read one field from a message as a Clojure value. nil means absent: an unset
  explicit-presence field, or an empty repeated/map field. IMPLICIT-presence
  fields (and proto3 no-label scalars) have no absence and return their value,
  default included. Nested messages come back as plain maps."
  ([msg handle] (get-field msg handle nil))
  ([msg ^FieldHandle handle opts]
   (let [^Message m msg
         fd ^Descriptors$FieldDescriptor (.-fd handle)]
     (cond
       (.-map? handle)
       (let [^FieldHandle kh (.-key-handle handle)
             ^FieldHandle vh (.-val-handle handle)]
         (if-let [g (.-get-invoker handle)]
           ;; getXMap: the builder's own map, no entry messages materialized.
           (let [^java.util.Map jm (.apply ^Function g m)]
             (when (pos? (.size jm))
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
       (let [^java.util.List vs (if-let [g (.-get-invoker handle)]
                                  (.apply ^Function g m)
                                  (.getField m fd))]
         (when (pos? (.size vs))
           (case (.-kind handle)
             ;; Scalars come back as they are: no per-element conversion.
             (:int :long :float :double :boolean :string) (vec vs)
             (persistent!
              (reduce (fn [acc v] (conj! acc (clj-value handle v opts)))
                      (transient [])
                      vs)))))

       :else
       (let [absent? (and (.-has-presence? handle)
                          (if-let [hs (.-has-invoker handle)]
                            (not (.apply ^Function hs m))
                            (not (.hasField m fd))))]
         (when-not absent?
           (clj-value handle
                      (if-let [g (.-get-invoker handle)]
                        (let [raw (.apply ^Function g m)]
                          (if (identical? :enum (.-kind handle))
                            (enum-of-number handle raw)
                            raw))
                        (.getField m fd))
                      opts)))))))
