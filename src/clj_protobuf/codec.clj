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
           [com.google.protobuf
            ByteString
            Descriptors$EnumDescriptor
            Descriptors$EnumValueDescriptor
            Descriptors$FieldDescriptor
            Message
            Message$Builder]))

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
          (keyword? v) (.findValueByName et (name v))
          (string? v)  (.findValueByName et ^String v)
          (number? v)  (or (.findValueByNumber et (int v))
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
        proto-keys? (= :proto (:naming opts))]
    (doseq [[kebab-k ^FieldHandle ch] @(.-children h)]
      (set-field! b ch (get m (if proto-keys? (.-proto-key ch) kebab-k)) opts))
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

(defn- set-map! [^Message$Builder b ^FieldHandle h m opts]
  (when-not (map? m) (type-mismatch h m "a map"))
  (let [fd ^Descriptors$FieldDescriptor (.-fd h)
        ^Message entry-proto (.-nested-prototype h)
        ^FieldHandle kh (.-key-handle h)
        ^FieldHandle vh (.-val-handle h)]
    (doseq [[k v] m]
      (let [eb (.newBuilderForType entry-proto)]
        (.setField eb (.-fd kh) (proto-value kh k opts))
        (.setField eb (.-fd vh) (proto-value vh v opts))
        (.addRepeatedField b fd (.build eb))))))

(defn- set-repeated! [^Message$Builder b ^FieldHandle h vs opts]
  (when-not (sequential? vs) (type-mismatch h vs "a sequential collection"))
  (let [fd ^Descriptors$FieldDescriptor (.-fd h)]
    (doseq [v vs]
      (.addRepeatedField b fd (proto-value h v opts)))))

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
         :else (.setField b ^Descriptors$FieldDescriptor (.-fd handle)
                          (proto-value handle v opts)))))
   builder))

(declare get-field)

(defn- message->map
  "A parsed nested message as a plain map. Only present fields appear; the
  consumer sees nil for the rest either way."
  [^FieldHandle h ^Message m opts]
  (let [proto-keys? (= :proto (:naming opts))]
    (reduce (fn [acc [kebab-k ^FieldHandle ch]]
              (let [v (get-field m ch opts)]
                (if (some? v)
                  (assoc acc (if proto-keys? (.-proto-key ch) kebab-k) v)
                  acc)))
            {}
            @(.-children h))))

(defn- clj-value
  [^FieldHandle h v opts]
  (case (.-kind h)
    (:int :long :float :double :boolean :string) v
    :bytes   (if (= :byte-string (:bytes opts))
               v
               (.toByteArray ^ByteString v))
    :enum    (case (:enums opts :keyword)
               :keyword (keyword (.getName ^Descriptors$EnumValueDescriptor v))
               :number  (.getNumber ^Descriptors$EnumValueDescriptor v)
               :string  (.getName ^Descriptors$EnumValueDescriptor v))
    :message (message->map h v opts)))

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
       (let [entries ^java.util.List (.getField m fd)]
         (when (pos? (.size entries))
           (let [^FieldHandle kh (.-key-handle handle)
                 ^FieldHandle vh (.-val-handle handle)]
             (persistent!
              (reduce (fn [acc ^Message e]
                        (assoc! acc
                                (clj-value kh (.getField e (.-fd kh)) opts)
                                (clj-value vh (.getField e (.-fd vh)) opts)))
                      (transient {})
                      entries)))))

       (.-repeated? handle)
       (let [vs ^java.util.List (.getField m fd)]
         (when (pos? (.size vs))
           (mapv #(clj-value handle % opts) vs)))

       :else
       (if (and (.-has-presence? handle) (not (.hasField m fd)))
         nil
         (clj-value handle (.getField m fd) opts))))))
