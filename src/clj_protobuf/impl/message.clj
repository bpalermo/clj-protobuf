(ns clj-protobuf.impl.message
  "The compiled message: three deftypes implementing protobuf-java's
  Message, Message.Builder and Parser interfaces over the compiler's slot
  layout, so that everything downstream — the generated code's
  `.newBuilderForType` / `.build`, `core/encode` and `core/decode`, grpc
  marshallers, TextFormat and JsonFormat — keeps working, with none of
  DynamicMessage's FieldSet behind it.

  A CompiledMessage is a compiled type, an Object[] of slots and an
  UnknownFieldSet (nil when empty). It is immutable; its serialized size is
  memoized. A CompiledBuilder is the same, mutable. Collections in slots are
  never mutated in place once a message may share them: a builder that has
  built, or was made by toBuilder, copies a collection before touching it.
  The parser owns fresh slots, so parsing mutates freely.

  The reflective API returns what protobuf-java's does — EnumValueDescriptor
  for enums, a list of entry messages for maps, the nested default instance
  for an unset message field — and equals/hashCode follow AbstractMessage's
  algorithm exactly, so a compiled message equals and hashes like a
  DynamicMessage of the same descriptor and value. Descriptors from another
  pool (a generated class) are never equal, by protobuf-java's own rule."
  (:require [clj-protobuf.impl.compile :as compile])
  (:import [clj_protobuf.impl.compile CompiledField CompiledType]
           [com.google.protobuf
            AbstractMessageLite
            ByteString
            CodedInputStream
            CodedOutputStream
            Descriptors$Descriptor
            Descriptors$EnumDescriptor
            Descriptors$EnumValueDescriptor
            Descriptors$FieldDescriptor
            Descriptors$FieldDescriptor$JavaType
            Descriptors$OneofDescriptor
            DynamicMessage
            ExtensionRegistryLite
            Internal$EnumLite
            InvalidProtocolBufferException
            Message
            Message$Builder
            MessageLite
            MessageLite$Builder
            Parser
            TextFormat
            UninitializedMessageException
            UnknownFieldSet
            UnknownFieldSet$Builder
            UnsafeByteOperations]
           [java.io InputStream OutputStream]
           [java.nio ByteBuffer]
           [java.util ArrayList Collections IdentityHashMap LinkedHashMap List Map Map$Entry TreeMap]))

(set! *warn-on-reflection* true)

(declare parser-for ->message ->builder)

;; ---------------------------------------------------------------------------
;; Reflective representations

(defn- enum-value
  ^Descriptors$EnumValueDescriptor [^Descriptors$EnumDescriptor et ^long n]
  (or (.findValueByNumber et (int n))
      (.findValueByNumberCreatingIfUnknown et (int n))))

(defn- enum-number
  "Anything the reflective API accepts for an enum, to the slot's number."
  ^long [v]
  (cond
    (instance? Internal$EnumLite v) (.getNumber ^Internal$EnumLite v)
    (number? v) (long v)
    :else (throw (IllegalArgumentException. (str "not an enum value: " (class v))))))

(defn- nested-type ^CompiledType [^CompiledField f] (deref (.-nested f)))

(defn- default-instance
  "A fresh default instance of a compiled type."
  ^Message [^CompiledType t]
  (->message t (object-array (compile/field-count t)) nil))

(defn- entry-descriptor ^Descriptors$Descriptor [^CompiledField f] (.getMessageType ^Descriptors$FieldDescriptor (.-fd f)))

(defn- to-reflective
  "A slot value as protobuf-java's reflective API represents it."
  [^CompiledField f v]
  (cond
    (.-map? f)
    ;; a list of entry messages, in insertion order
    (let [ed (entry-descriptor f)
          kfd (.findFieldByName ed "key")
          vfd (.findFieldByName ed "value")
          ^Descriptors$EnumDescriptor vet (.-val-enum-type f)
          out (ArrayList. (.size ^Map v))
          it (.iterator (.entrySet ^Map v))]
      (while (.hasNext it)
        (let [^Map$Entry e (.next it)
              x (.getValue e)]
          (.add out (-> (DynamicMessage/newBuilder ed)
                        (.setField kfd (.getKey e))
                        (.setField vfd (if vet (enum-value vet (long x)) x))
                        (.build)))))
      (Collections/unmodifiableList out))

    (.-repeated? f)
    (if (= :enum (.-kind f))
      (let [^Descriptors$EnumDescriptor et (.-enum-type f)
            ^List l v
            out (ArrayList. (.size l))]
        (dotimes [i (.size l)] (.add out (enum-value et (long (.get l i)))))
        (Collections/unmodifiableList out))
      (Collections/unmodifiableList ^List v))

    (= :enum (.-kind f)) (enum-value (.-enum-type f) (long v))
    :else v))

(defn- reflective-default
  "What getField returns for an absent field."
  [^CompiledField f]
  (cond
    (or (.-map? f) (.-repeated? f)) (Collections/emptyList)
    (= :message (.-kind f)) (default-instance (nested-type f))
    :else (.getDefaultValue ^Descriptors$FieldDescriptor (.-fd f))))

(defn- from-reflective
  "A reflective-API value to slot representation; nil for a value that
  means absence on a field without presence."
  [^CompiledField f v]
  (let [slot (cond
               (.-map? f)
               (let [ed (entry-descriptor f)
                     kfd (.findFieldByName ed "key")
                     vfd (.findFieldByName ed "value")
                     enum-vals? (some? (.-val-enum-type f))
                     m (LinkedHashMap.)]
                 (if (instance? Map v)
                   (let [it (.iterator (.entrySet ^Map v))]
                     (while (.hasNext it)
                       (let [^Map$Entry e (.next it)
                             x (.getValue e)]
                         (.put m (.getKey e) (if enum-vals? (Integer/valueOf (int (enum-number x))) x)))))
                   (doseq [^Message e ^List v]
                     (let [x (.getField e vfd)]
                       (.put m (.getField e kfd) (if enum-vals? (Integer/valueOf (int (enum-number x))) x)))))
                 m)

               (.-repeated? f)
               (let [^List l v
                     out (ArrayList. (.size l))]
                 (if (= :enum (.-kind f))
                   (dotimes [i (.size l)] (.add out (Integer/valueOf (int (enum-number (.get l i))))))
                   (.addAll out l))
                 out)

               (= :enum (.-kind f)) (Integer/valueOf (int (enum-number v)))
               (= :bytes (.-kind f)) (if (bytes? v) (ByteString/copyFrom ^bytes v) v)
               :else v)]
    (if (and (not (.-has-presence? f))
             (not (.-map? f))
             (not (.-repeated? f))
             (not= :message (.-kind f))
             (.equals ^Object (.-default f) slot))
      nil
      slot)))

;; ---------------------------------------------------------------------------
;; Reads shared by message and builder

(defn- field-of ^CompiledField [^CompiledType t ^Descriptors$FieldDescriptor fd]
  (when-not (identical? (.getContainingType fd) (.-descriptor t))
    (throw (IllegalArgumentException.
            (str "FieldDescriptor does not match message type: " (.getFullName fd)
                 " is not a field of " (.getFullName ^Descriptors$Descriptor (.-descriptor t))))))
  (aget ^objects (.-fields t) (.getIndex fd)))

(defn- present?
  [^CompiledField f v]
  (and (some? v)
       (cond (.-repeated? f) (not (.isEmpty ^List v))
             (.-map? f) (not (.isEmpty ^Map v))
             :else true)))

(defn- get-field [^CompiledType t ^objects slots ^Descriptors$FieldDescriptor fd]
  (let [f (field-of t fd)
        v (aget slots (.-slot f))]
    (if (present? f v) (to-reflective f v) (reflective-default f))))

(defn- has-field [^CompiledType t ^objects slots ^Descriptors$FieldDescriptor fd]
  (let [f (field-of t fd)]
    (when (or (.-repeated? f) (.-map? f))
      (throw (UnsupportedOperationException. "hasField() can only be called on non-repeated fields.")))
    (some? (aget slots (.-slot f)))))

(defn- all-fields
  "Present fields by descriptor, sorted by field number as protobuf-java's
  getAllFields is."
  ^Map [^CompiledType t ^objects slots]
  (let [^objects fields (.-fields t)
        m (TreeMap.)]
    (dotimes [i (alength fields)]
      (let [^CompiledField f (aget fields i)
            v (aget slots i)]
        (when (present? f v)
          (.put m (.-fd f) (to-reflective f v)))))
    (Collections/unmodifiableMap m)))

(defn- repeated-count ^long [^CompiledType t ^objects slots ^Descriptors$FieldDescriptor fd]
  (let [f (field-of t fd)
        v (aget slots (.-slot f))]
    (cond
      (.-repeated? f) (if v (.size ^List v) 0)
      (.-map? f) (if v (.size ^Map v) 0)
      :else (throw (UnsupportedOperationException. "getRepeatedFieldCount() can only be called on repeated fields.")))))

(defn- repeated-field [^CompiledType t ^objects slots ^Descriptors$FieldDescriptor fd ^long i]
  (let [f (field-of t fd)]
    (.get ^List (get-field t slots fd) (int i))))

(defn- oneof-field
  "The set member of a oneof, or nil."
  ^Descriptors$FieldDescriptor [^CompiledType t ^objects slots ^Descriptors$OneofDescriptor oo]
  (some (fn [^Descriptors$FieldDescriptor fd]
          (when (some? (aget slots (.getIndex fd))) fd))
        (.getFields oo)))

;; ---------------------------------------------------------------------------
;; Initialization: required fields, recursively — but only where a required
;; field can exist. Types with none in their transitive closure (every proto3
;; and editions-default message) skip the walk.

(def ^:private required-somewhere
  (Collections/synchronizedMap (IdentityHashMap.)))

(defn- required-somewhere?
  ([^CompiledType t] (required-somewhere? t #{}))
  ([^CompiledType t visiting]
   (if-some [known (.get ^Map required-somewhere t)]
     known
     (if (contains? visiting t)
       false
       (let [visiting (conj visiting t)
             ^objects fields (.-fields t)
             answer (boolean
                     (or (pos? (alength ^ints (.-required-slots t)))
                         (some (fn [^CompiledField f]
                                 (and (.-nested f) (required-somewhere? (nested-type f) visiting)))
                               fields)))]
         (.put ^Map required-somewhere t answer)
         answer)))))

(defn- initialized? [^CompiledType t ^objects slots]
  (or (not (required-somewhere? t))
      (let [^ints req (.-required-slots t)
            ^objects fields (.-fields t)]
        (and (loop [i 0]
               (if (< i (alength req))
                 (and (some? (aget slots (aget req i))) (recur (unchecked-inc i)))
                 true))
             (loop [i 0]
               (if (< i (alength fields))
                 (let [^CompiledField f (aget fields i)
                       v (aget slots i)]
                   (and (or (nil? v)
                            (nil? (.-nested f))
                            (cond
                              (.-repeated? f) (every? (fn [^MessageLite m] (.isInitialized m)) ^List v)
                              (.-map? f) (every? (fn [^MessageLite m] (.isInitialized m)) (.values ^Map v))
                              :else (.isInitialized ^MessageLite v)))
                        (recur (unchecked-inc i))))
                 true))))))

(defn- initialization-errors
  "Paths of missing required fields, as MessageReflection spells them."
  [^CompiledType t ^objects slots prefix]
  (let [^objects fields (.-fields t)]
    (into []
          (mapcat (fn [^CompiledField f]
                    (let [v (aget slots (.-slot f))
                          n (.getName ^Descriptors$FieldDescriptor (.-fd f))]
                      (cond
                        (and (.-required? f) (nil? v)) [(str prefix n)]
                        (or (nil? v) (nil? (.-nested f))) nil
                        (.-repeated? f) (mapcat (fn [i ^Message m]
                                                  (when-not (.isInitialized m)
                                                    (map #(str prefix n "[" i "]." %) (.findInitializationErrors m))))
                                                (range) ^List v)
                        (.-map? f) (mapcat (fn [[k ^Message m]]
                                             (when-not (.isInitialized m)
                                               (map #(str prefix n "[" k "]." %) (.findInitializationErrors m))))
                                           ^Map v)
                        :else (when-not (.isInitialized ^Message v)
                                (map #(str prefix n "." %) (.findInitializationErrors ^Message v)))))))
          fields)))

;; ---------------------------------------------------------------------------
;; Equality and hashing, AbstractMessage's algorithm

(defn- map-of-entries
  "A map field's reflective value (entry messages) as a Map."
  ^Map [^Descriptors$FieldDescriptor fd entries]
  (let [ed (.getMessageType fd)
        kfd (.findFieldByName ed "key")
        vfd (.findFieldByName ed "value")
        m (java.util.HashMap.)]
    (doseq [^Message e ^List entries]
      (.put m (.getField e kfd) (.getField e vfd)))
    m))

(defn- fields-equal? [^Map a ^Map b]
  (and (= (.size a) (.size b))
       (every? (fn [^Map$Entry e]
                 (let [^Descriptors$FieldDescriptor fd (.getKey e)
                       x (.getValue e)
                       y (.get b fd)]
                   (and (.containsKey b fd)
                        (if (.isMapField fd)
                          (.equals (map-of-entries fd x) (map-of-entries fd y))
                          (.equals ^Object x y)))))
               (.entrySet a))))

(defn- hash-map-key-or-value ^long [o]
  (if (instance? Internal$EnumLite o)
    (.getNumber ^Internal$EnumLite o)
    (.hashCode ^Object o)))

(defn- hash-fields ^long [^long hash ^Map fields]
  (reduce (fn [^long h ^Map$Entry e]
            (let [^Descriptors$FieldDescriptor fd (.getKey e)
                  v (.getValue e)
                  h (unchecked-add-int (unchecked-multiply-int 37 h) (.getNumber fd))]
              (cond
                (.isMapField fd)
                (unchecked-add-int (unchecked-multiply-int 53 h)
                                   (int (reduce (fn [^long acc ^Map$Entry me]
                                                  (unchecked-add-int acc (bit-xor (hash-map-key-or-value (.getKey me))
                                                                                  (hash-map-key-or-value (.getValue me)))))
                                                0
                                                (.entrySet (map-of-entries fd v)))))
                (= Descriptors$FieldDescriptor$JavaType/ENUM (.getJavaType fd))
                (if (.isRepeated fd)
                  (unchecked-add-int (unchecked-multiply-int 53 h)
                                     (int (reduce (fn [^long acc ^Internal$EnumLite x]
                                                    (unchecked-add-int (unchecked-multiply-int 31 acc) (.getNumber x)))
                                                  1 ^List v)))
                  (unchecked-add-int (unchecked-multiply-int 53 h) (.getNumber ^Internal$EnumLite v)))
                :else
                (unchecked-add-int (unchecked-multiply-int 53 h) (.hashCode ^Object v)))))
          hash
          (.entrySet fields)))

(defn- message-hash ^long [^CompiledType t ^objects slots ^UnknownFieldSet unknown]
  (let [h (unchecked-add-int (unchecked-multiply-int 19 41) (.hashCode ^Descriptors$Descriptor (.-descriptor t)))
        h (hash-fields h (all-fields t slots))]
    (unchecked-add-int (unchecked-multiply-int 29 h) (.hashCode unknown))))

(defn- message-equals? [^CompiledType t ^objects slots ^UnknownFieldSet unknown o]
  (and (instance? Message o)
       (let [^Message m o]
         (and (identical? (.-descriptor t) (.getDescriptorForType m))
              (fields-equal? (all-fields t slots) (.getAllFields m))
              (.equals unknown (.getUnknownFields m))))))

;; ---------------------------------------------------------------------------
;; Writing

(defn- unknown-or-empty ^UnknownFieldSet [unknown]
  (or unknown (UnknownFieldSet/getDefaultInstance)))

(defn- preferred-buffer-size
  "AbstractMessageLite's rule (the method is package-private): the data
  length, capped at 4096."
  ^long [^long n]
  (if (> n 4096) 4096 (max n 1)))

(defn- write-to-stream [^MessageLite m ^OutputStream out]
  (let [size (.getSerializedSize m)
        cos (CodedOutputStream/newInstance out (int (preferred-buffer-size size)))]
    (.writeTo m cos)
    (.flush cos)))

(defn- write-delimited [^MessageLite m ^OutputStream out]
  (let [size (.getSerializedSize m)
        cos (CodedOutputStream/newInstance out (int (preferred-buffer-size
                                                     (unchecked-add-int (CodedOutputStream/computeUInt32SizeNoTag size) size))))]
    (.writeUInt32NoTag cos size)
    (.writeTo m cos)
    (.flush cos)))

(defn- to-byte-array ^bytes [^MessageLite m]
  (let [size (.getSerializedSize m)
        bs (byte-array size)
        cos (CodedOutputStream/newInstance bs)]
    (.writeTo m cos)
    (.checkNoSpaceLeft cos)
    bs))

(defn- to-byte-string ^ByteString [^MessageLite m]
  ;; The array is ours and never shared, so wrapping it is safe.
  (UnsafeByteOperations/unsafeWrap (to-byte-array m)))

;; ---------------------------------------------------------------------------
;; The message

(deftype CompiledMessage [^CompiledType type
                          ^objects slots
                          ^UnknownFieldSet unknown
                          ;; -1 until computed. Two threads may compute it
                          ;; at once and both store the same value: benign.
                          ^:unsynchronized-mutable ^long memoized-size]
  Message
  (getDescriptorForType [_] (.-descriptor type))
  (getDefaultInstanceForType [_] (default-instance type))
  (getAllFields [_] (all-fields type slots))
  (hasField [_ fd] (has-field type slots fd))
  (getField [_ fd] (get-field type slots fd))
  (getRepeatedFieldCount [_ fd] (int (repeated-count type slots fd)))
  (getRepeatedField [_ fd i] (repeated-field type slots fd i))
  (hasOneof [_ oo] (some? (oneof-field type slots oo)))
  (getOneofFieldDescriptor [_ oo] (oneof-field type slots oo))
  (getUnknownFields [_] (unknown-or-empty unknown))
  (isInitialized [_] (initialized? type slots))
  (findInitializationErrors [_] (initialization-errors type slots ""))
  (getInitializationErrorString [this]
    (clojure.string/join ", " (.findInitializationErrors this)))
  (^void writeTo [_ ^CodedOutputStream out] (compile/write-slots type slots unknown out))
  (^void writeTo [this ^OutputStream out] (write-to-stream this out))
  (writeDelimitedTo [this out] (write-delimited this out))
  (getSerializedSize [_]
    (let [s memoized-size]
      (if (>= s 0)
        (int s)
        (let [s (compile/size-slots type slots unknown)]
          (set! memoized-size s)
          (int s)))))
  (toByteArray [this] (to-byte-array this))
  (toByteString [this] (to-byte-string this))
  (getParserForType [_] (parser-for type))
  (newBuilderForType [_] (->builder type (object-array (compile/field-count type)) nil false))
  (toBuilder [_] (->builder type (aclone slots) unknown true))

  Object
  (equals [_ o] (message-equals? type slots (unknown-or-empty unknown) o))
  (hashCode [_] (int (message-hash type slots (unknown-or-empty unknown))))
  (toString [this] (.printToString (TextFormat/printer) this)))

(defn ->message ^CompiledMessage [^CompiledType t ^objects slots ^UnknownFieldSet unknown]
  (CompiledMessage. t slots unknown -1))

;; ---------------------------------------------------------------------------
;; The builder

(defn- copy-collection [v]
  (cond
    (instance? ArrayList v) (ArrayList. ^ArrayList v)
    (instance? LinkedHashMap v) (LinkedHashMap. ^LinkedHashMap v)
    :else v))

(defn- merge-slot
  "protobuf's mergeFrom semantics for one slot: scalars overwrite, messages
  merge, repeateds append, maps put."
  [^CompiledField f current incoming]
  (cond
    (nil? incoming) current
    (.-repeated? f) (let [^ArrayList l (if current (ArrayList. ^List current) (ArrayList.))]
                      (.addAll l ^List incoming) l)
    (.-map? f) (let [^LinkedHashMap m (if current (LinkedHashMap. ^Map current) (LinkedHashMap.))]
                 (.putAll m ^Map incoming) m)
    (= :message (.-kind f)) (if current
                              (-> (.toBuilder ^Message current) (.mergeFrom ^Message incoming) (.buildPartial))
                              incoming)
    :else incoming))

(deftype CompiledBuilder [^CompiledType type
                          ^objects slots
                          ^:unsynchronized-mutable ^UnknownFieldSet unknown
                          ;; true when a message may share our collections
                          ^:unsynchronized-mutable shared]
  Message$Builder
  ;; --- reads: the same as the message's
  (getDescriptorForType [_] (.-descriptor type))
  (getDefaultInstanceForType [_] (default-instance type))
  (getAllFields [_] (all-fields type slots))
  (hasField [_ fd] (has-field type slots fd))
  (getField [_ fd] (get-field type slots fd))
  (getRepeatedFieldCount [_ fd] (int (repeated-count type slots fd)))
  (getRepeatedField [_ fd i] (repeated-field type slots fd i))
  (hasOneof [_ oo] (some? (oneof-field type slots oo)))
  (getOneofFieldDescriptor [_ oo] (oneof-field type slots oo))
  (getUnknownFields [_] (unknown-or-empty unknown))
  (isInitialized [_] (initialized? type slots))
  (findInitializationErrors [_] (initialization-errors type slots ""))
  (getInitializationErrorString [this]
    (clojure.string/join ", " (.findInitializationErrors this)))

  ;; --- building
  (build [this]
    (when-not (initialized? type slots)
      (throw (UninitializedMessageException. ^List (initialization-errors type slots ""))))
    (.buildPartial this))
  (buildPartial [_]
    (set! shared true)
    (->message type (aclone slots) unknown))
  (clear [this]
    (java.util.Arrays/fill slots nil)
    (set! unknown nil)
    this)
  (clone [_] (->builder type (aclone slots) unknown true))

  ;; --- writes
  (setField [this fd v]
    (let [^CompiledField f (field-of type fd)]
      (aset slots (.-slot f) (from-reflective f v))
      (when (>= (.-oneof f) 0)
        (let [^ints siblings (aget ^objects (.-oneof-slots type) (.-oneof f))]
          (dotimes [i (alength siblings)]
            (let [s (aget siblings i)]
              (when (not= s (.-slot f)) (aset slots s nil))))))
      this))
  (clearField [this fd]
    (aset slots (.-slot (field-of type fd)) nil)
    this)
  (clearOneof [this oo]
    (doseq [^Descriptors$FieldDescriptor fd (.getFields oo)]
      (aset slots (.getIndex fd) nil))
    this)
  (setRepeatedField [this fd i v]
    (let [^CompiledField f (field-of type fd)]
      (when-not (.-repeated? f)
        (throw (UnsupportedOperationException. "setRepeatedField() can only be called on repeated fields.")))
      (let [^List cur (or (aget slots (.-slot f)) (ArrayList.))
            l (ArrayList. cur)]
        (.set l (int i) (if (= :enum (.-kind f)) (Integer/valueOf (int (enum-number v))) v))
        (aset slots (.-slot f) l))
      this))
  (addRepeatedField [this fd v]
    (let [^CompiledField f (field-of type fd)]
      (cond
        (.-repeated? f)
        (let [^ArrayList l (if-let [cur (aget slots (.-slot f))]
                             (if shared (ArrayList. ^List cur) cur)
                             (ArrayList.))]
          (.add l (if (= :enum (.-kind f)) (Integer/valueOf (int (enum-number v))) v))
          (aset slots (.-slot f) l))

        (.-map? f)
        ;; one entry message, as the reflective API adds map entries
        (let [^Message e v
              ed (entry-descriptor f)
              x (.getField e (.findFieldByName ed "value"))
              ^LinkedHashMap m (if-let [cur (aget slots (.-slot f))]
                                 (if shared (LinkedHashMap. ^Map cur) cur)
                                 (LinkedHashMap.))]
          (.put m (.getField e (.findFieldByName ed "key"))
                (if (.-val-enum-type f) (Integer/valueOf (int (enum-number x))) x))
          (aset slots (.-slot f) m))

        :else (throw (UnsupportedOperationException. "addRepeatedField() can only be called on repeated fields.")))
      this))
  (setUnknownFields [this u] (set! unknown u) this)
  (mergeUnknownFields [this u]
    (set! unknown (if unknown (-> (UnknownFieldSet/newBuilder unknown) (.mergeFrom ^UnknownFieldSet u) (.build)) u))
    this)
  (newBuilderForField [_ fd]
    (let [^CompiledField f (field-of type fd)]
      (cond
        (.-map? f) (DynamicMessage/newBuilder (entry-descriptor f))
        (= :message (.-kind f)) (let [^CompiledType nt (nested-type f)]
                                  (->builder nt (object-array (compile/field-count nt)) nil false))
        :else (throw (UnsupportedOperationException. "newBuilderForField() called on a non-message field.")))))
  (getFieldBuilder [_ _] (throw (UnsupportedOperationException. "getFieldBuilder() is not supported for compiled messages.")))
  (getRepeatedFieldBuilder [_ _ _] (throw (UnsupportedOperationException. "getRepeatedFieldBuilder() is not supported for compiled messages.")))

  ;; --- merging
  (^com.google.protobuf.Message$Builder mergeFrom [this ^Message other]
    (if (and (instance? CompiledMessage other) (identical? type (.-type ^CompiledMessage other)))
      (let [^objects theirs (.-slots ^CompiledMessage other)
            ^objects fields (.-fields type)]
        (dotimes [i (alength fields)]
          (let [^CompiledField f (aget fields i)
                incoming (aget theirs i)]
            (when (some? incoming)
              (aset slots i (merge-slot f (aget slots i) incoming))
              (when (>= (.-oneof f) 0)
                (.clearOneof this (.getRealContainingOneof ^Descriptors$FieldDescriptor (.-fd f)))
                (aset slots i (merge-slot f nil incoming)))))))
      (do
        (when-not (identical? (.-descriptor type) (.getDescriptorForType other))
          (throw (IllegalArgumentException. "mergeFrom(Message) can only merge messages of the same type.")))
        (doseq [^Map$Entry e (.entrySet (.getAllFields other))]
          (let [^Descriptors$FieldDescriptor fd (.getKey e)
                ^CompiledField f (field-of type fd)]
            (if (>= (.-oneof f) 0)
              (.setField this fd (.getValue e))
              (aset slots (.-slot f) (merge-slot f (aget slots (.-slot f)) (from-reflective f (.getValue e)))))))))
    (.mergeUnknownFields this (.getUnknownFields other))
    this)
  (^com.google.protobuf.MessageLite$Builder mergeFrom [this ^MessageLite other]
    (.mergeFrom this ^Message other))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^CodedInputStream in]
    (.mergeFrom this in (ExtensionRegistryLite/getEmptyRegistry)))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^CodedInputStream in ^ExtensionRegistryLite _]
    (when shared
      (dotimes [i (alength slots)] (aset slots i (copy-collection (aget slots i))))
      (set! shared false))
    (when-let [^UnknownFieldSet$Builder u (compile/read-into type in slots)]
      (.mergeUnknownFields this (.build u)))
    this)
  (^com.google.protobuf.Message$Builder mergeFrom [this ^ByteString bs] (.mergeFrom this (.newCodedInput bs)))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^ByteString bs ^ExtensionRegistryLite r] (.mergeFrom this (.newCodedInput bs) r))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^bytes bs] (.mergeFrom this (CodedInputStream/newInstance bs)))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^bytes bs ^ExtensionRegistryLite r] (.mergeFrom this (CodedInputStream/newInstance bs) r))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^bytes bs ^int off ^int len] (.mergeFrom this (CodedInputStream/newInstance bs off len)))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^bytes bs ^int off ^int len ^ExtensionRegistryLite r]
    (.mergeFrom this (CodedInputStream/newInstance bs off len) r))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^InputStream in] (.mergeFrom this (CodedInputStream/newInstance in)))
  (^com.google.protobuf.Message$Builder mergeFrom [this ^InputStream in ^ExtensionRegistryLite r] (.mergeFrom this (CodedInputStream/newInstance in) r))
  (^boolean mergeDelimitedFrom [this ^InputStream in] (.mergeDelimitedFrom this in (ExtensionRegistryLite/getEmptyRegistry)))
  (^boolean mergeDelimitedFrom [this ^InputStream in ^ExtensionRegistryLite r]
    (let [first-byte (.read in)]
      (if (= -1 first-byte)
        false
        (let [size (CodedInputStream/readRawVarint32 first-byte in)
              bs (.readNBytes in size)]
          (when (< (alength bs) size)
            (throw (InvalidProtocolBufferException. "While parsing a protocol message, the input ended unexpectedly in the middle of a field.")))
          (.mergeFrom this bs r)
          true)))))

(defn ->builder ^CompiledBuilder [^CompiledType t ^objects slots ^UnknownFieldSet unknown shared?]
  (CompiledBuilder. t slots unknown (boolean shared?)))

;; ---------------------------------------------------------------------------
;; The parser: every overload through one parsePartialFrom, plus the
;; initialization check for the non-partial ones.

(defn- parse-partial ^CompiledMessage [^CompiledType t ^CodedInputStream in]
  (let [slots (object-array (compile/field-count t))
        unknown (compile/read-into t in slots)]
    (->message t slots (some-> unknown .build))))

(defn- check-initialized ^CompiledMessage [^CompiledMessage m]
  (if (.isInitialized m)
    m
    (throw (-> (UninitializedMessageException. ^MessageLite m)
               (.asInvalidProtocolBufferException)
               (.setUnfinishedMessage m)))))

(defn- parse-complete
  "Parse and require the whole input consumed, as parseFrom does."
  ^CompiledMessage [^CompiledType t ^CodedInputStream in]
  (let [m (parse-partial t in)]
    (.checkLastTagWas in 0)
    m))

(defn- read-delimited-bytes
  "The next length-prefixed message's bytes from a stream, or nil at end."
  ^bytes [^InputStream in]
  (let [first-byte (.read in)]
    (when-not (= -1 first-byte)
      (let [size (CodedInputStream/readRawVarint32 first-byte in)
            bs (.readNBytes in size)]
        (when (< (alength bs) size)
          (throw (InvalidProtocolBufferException. "While parsing a protocol message, the input ended unexpectedly in the middle of a field.")))
        bs))))

(deftype CompiledParser [^CompiledType type]
  Parser
  (parsePartialFrom [_ ^CodedInputStream in] (parse-partial type in))
  (parsePartialFrom [_ ^CodedInputStream in ^ExtensionRegistryLite _] (parse-partial type in))
  (parseFrom [_ ^CodedInputStream in] (check-initialized (parse-partial type in)))
  (parseFrom [_ ^CodedInputStream in ^ExtensionRegistryLite _] (check-initialized (parse-partial type in)))
  (parseFrom [_ ^ByteBuffer b] (check-initialized (parse-complete type (CodedInputStream/newInstance b))))
  (parseFrom [_ ^ByteBuffer b ^ExtensionRegistryLite _] (check-initialized (parse-complete type (CodedInputStream/newInstance b))))
  (parseFrom [_ ^ByteString bs] (check-initialized (parse-complete type (.newCodedInput bs))))
  (parseFrom [_ ^ByteString bs ^ExtensionRegistryLite _] (check-initialized (parse-complete type (.newCodedInput bs))))
  (parsePartialFrom [_ ^ByteString bs] (parse-complete type (.newCodedInput bs)))
  (parsePartialFrom [_ ^ByteString bs ^ExtensionRegistryLite _] (parse-complete type (.newCodedInput bs)))
  (parseFrom [_ ^bytes bs] (check-initialized (parse-complete type (CodedInputStream/newInstance bs))))
  (parseFrom [_ ^bytes bs ^ExtensionRegistryLite _] (check-initialized (parse-complete type (CodedInputStream/newInstance bs))))
  (parseFrom [_ ^bytes bs ^int off ^int len] (check-initialized (parse-complete type (CodedInputStream/newInstance bs off len))))
  (parseFrom [_ ^bytes bs ^int off ^int len ^ExtensionRegistryLite _]
    (check-initialized (parse-complete type (CodedInputStream/newInstance bs off len))))
  (parsePartialFrom [_ ^bytes bs] (parse-complete type (CodedInputStream/newInstance bs)))
  (parsePartialFrom [_ ^bytes bs ^ExtensionRegistryLite _] (parse-complete type (CodedInputStream/newInstance bs)))
  (parsePartialFrom [_ ^bytes bs ^int off ^int len] (parse-complete type (CodedInputStream/newInstance bs off len)))
  (parsePartialFrom [_ ^bytes bs ^int off ^int len ^ExtensionRegistryLite _]
    (parse-complete type (CodedInputStream/newInstance bs off len)))
  (parseFrom [_ ^InputStream in] (check-initialized (parse-complete type (CodedInputStream/newInstance in))))
  (parseFrom [_ ^InputStream in ^ExtensionRegistryLite _] (check-initialized (parse-complete type (CodedInputStream/newInstance in))))
  (parsePartialFrom [_ ^InputStream in] (parse-complete type (CodedInputStream/newInstance in)))
  (parsePartialFrom [_ ^InputStream in ^ExtensionRegistryLite _] (parse-complete type (CodedInputStream/newInstance in)))
  (parseDelimitedFrom [this ^InputStream in] (.parseDelimitedFrom this in (ExtensionRegistryLite/getEmptyRegistry)))
  (parseDelimitedFrom [_ ^InputStream in ^ExtensionRegistryLite _]
    (when-let [bs (read-delimited-bytes in)]
      (check-initialized (parse-complete type (CodedInputStream/newInstance bs)))))
  (parsePartialDelimitedFrom [this ^InputStream in] (.parsePartialDelimitedFrom this in (ExtensionRegistryLite/getEmptyRegistry)))
  (parsePartialDelimitedFrom [_ ^InputStream in ^ExtensionRegistryLite _]
    (when-let [bs (read-delimited-bytes in)]
      (parse-complete type (CodedInputStream/newInstance bs)))))

;; ---------------------------------------------------------------------------
;; Per-type parser registry, and the compiler wired to it

(def ^:private parsers (Collections/synchronizedMap (IdentityHashMap.)))

(defn parser-for
  "The one Parser for a compiled type."
  ^Parser [^CompiledType t]
  (or (.get ^Map parsers t)
      (locking parsers
        (or (.get ^Map parsers t)
            (let [p (CompiledParser. t)]
              (.put ^Map parsers t p)
              p)))))

(def compile
  "Descriptor -> CompiledType, with this layer's parser for nested types."
  (compile/compiler (fn [ct] (delay (parser-for (deref ct))))))

(defn prototype
  "The default instance for a descriptor: what rt/message returns on the
  compiled arm."
  ^Message [^Descriptors$Descriptor d]
  (default-instance (compile d)))
