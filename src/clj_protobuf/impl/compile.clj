(ns clj-protobuf.impl.compile
  "The codec compiler: a Descriptor walked once into everything the hot
  path needs, so that parsing and writing never touch the descriptor API,
  never resolve a FieldDescriptor, and never ask protobuf-java what an
  edition feature means — those questions are answered here, per type, the
  first time the type is used.

  A message value is an Object[] of slots, one per field in descriptor
  order (FieldDescriptor.getIndex), plus an UnknownFieldSet. nil is absent.
  Fields without presence (proto3 scalars, editions IMPLICIT) are normalized
  so nil also means the default: readers and setters store nil for the
  default value, and reads substitute the default back. That keeps the
  writer's rule one line — write what is non-nil and non-empty — and makes
  re-encoding bytes that carried an explicit default drop it, as every
  protobuf implementation does.

  Compiled per type:
  - `fields`: a CompiledField per slot, with the writer for that field.
  - `writers`: the same fields in field-number order, the order every
    protobuf serializer emits, followed by unknown fields.
  - a reader table keyed by TAG, not field number, dense (an array indexed
    by tag) when the tags allow and binary-searched otherwise. A repeated
    scalar registers both its packed and its expanded tag, because a parser
    accepts either encoding whatever the descriptor says it writes.
  - oneof membership, so reading a member off the wire clears its siblings,
    and the required slots, for initialization checks.

  Nested types compile lazily through the parser-fn the message layer
  supplies, behind an IDeref, because descriptors are cyclic. One compiler
  owns one cache, keyed by Descriptor identity."
  (:require [clj-protobuf.impl.wire :as wire])
  (:import [clj_protobuf.impl.wire FieldReader FieldWriter]
           [com.google.protobuf
            ByteString
            CodedInputStream
            CodedOutputStream
            Descriptors$Descriptor
            Descriptors$EnumDescriptor
            Descriptors$EnumValueDescriptor
            Descriptors$FieldDescriptor
            Descriptors$FieldDescriptor$JavaType
            Descriptors$FieldDescriptor$Type
            Descriptors$OneofDescriptor
            Parser
            UnknownFieldSet
            UnknownFieldSet$Builder
            WireFormat]
           [java.util ArrayList Arrays List Map]
           [java.util.concurrent ConcurrentHashMap]))

(set! *warn-on-reflection* true)

(defrecord CompiledField
           [^Descriptors$FieldDescriptor fd
            ^long slot
            ^long number
            kind            ; :int :long :float :double :boolean :string :bytes :enum :message
            repeated?       ; non-map repeated
            map?
            group?          ; proto2 group / editions DELIMITED
            has-presence?
            required?
            default         ; slot-representation default, scalars only (nil for message/collection)
            ^long oneof     ; index into the type's real oneofs, or -1
            ^FieldWriter writer
            ^Descriptors$EnumDescriptor enum-type
            key-type        ; map key type name
            val-type        ; map value type name
            val-enum-type   ; map value EnumDescriptor, enum-valued maps only
            nested])        ; delay of the nested CompiledType (message kind; map: the value type)

(defrecord CompiledType
           [^Descriptors$Descriptor descriptor
            ^objects fields          ; CompiledField per slot
            ^objects writers         ; CompiledField in field-number order
            ^ints tags               ; sorted tags, sparse table only
            ^ints tag-slots          ; slot per table position
            ^objects tag-readers     ; FieldReader per table position
            ^ints tag-oneofs         ; oneof index per table position, -1 if none
            ^long dense              ; table size when indexed by tag directly, else 0
            ^objects oneof-slots     ; int[] of slots per real oneof
            ^ints required-slots
            needs-unknown?])         ; a reader routes into unknown fields (closed enums)

;; ---------------------------------------------------------------------------
;; Per-field analysis

(defn- kind-of [^Descriptors$FieldDescriptor fd]
  (condp = (.getJavaType fd)
    Descriptors$FieldDescriptor$JavaType/INT         :int
    Descriptors$FieldDescriptor$JavaType/LONG        :long
    Descriptors$FieldDescriptor$JavaType/FLOAT       :float
    Descriptors$FieldDescriptor$JavaType/DOUBLE      :double
    Descriptors$FieldDescriptor$JavaType/BOOLEAN     :boolean
    Descriptors$FieldDescriptor$JavaType/STRING      :string
    Descriptors$FieldDescriptor$JavaType/BYTE_STRING :bytes
    Descriptors$FieldDescriptor$JavaType/ENUM        :enum
    Descriptors$FieldDescriptor$JavaType/MESSAGE     :message))

(defn- type-name ^String [^Descriptors$FieldDescriptor fd] (.name (.getType fd)))

(defn- group? [^Descriptors$FieldDescriptor fd]
  ;; protobuf-java reports editions DELIMITED as GROUP once features resolve.
  (= Descriptors$FieldDescriptor$Type/GROUP (.getType fd)))

(defn slot-default
  "A scalar field's default in slot representation: protobuf-java's default
  value, except enums, which are numbers here."
  [^Descriptors$FieldDescriptor fd]
  (when (and (not (.isRepeated fd)) (not= :message (kind-of fd)))
    (let [d (.getDefaultValue fd)]
      (if (instance? Descriptors$EnumValueDescriptor d)
        (Integer/valueOf (.getNumber ^Descriptors$EnumValueDescriptor d))
        d))))

(defn- known-numbers
  "For a closed enum, the set of declared numbers; nil for open enums,
  whose every number is a value."
  [^Descriptors$EnumDescriptor et]
  (when (.isClosed et)
    (into #{} (map (fn [^Descriptors$EnumValueDescriptor v] (.getNumber v))) (.getValues et))))

(defn- scalar-opts [^Descriptors$FieldDescriptor fd]
  (cond-> {}
    (= :string (kind-of fd)) (assoc :utf8? (.needsUtf8Check fd))
    (= :enum (kind-of fd)) (merge (when-let [known (known-numbers (.getEnumType fd))]
                                    {:known? known :field-number (.getNumber fd)}))))

(defn- normalized-reader
  "Fields without presence: the default value on the wire is stored as nil."
  ^FieldReader [^FieldReader inner ^Object default]
  (reify FieldReader
    (read [_ in current unknown]
      (let [v (.read inner in current unknown)]
        (if (.equals default v) nil v)))))

(defn- scalar-readers
  "[[tag reader] ...] for a scalar field: one entry when singular, two when
  repeated (its own encoding and the other one)."
  [^Descriptors$FieldDescriptor fd ^CompiledField f]
  (let [tn (type-name fd)
        n (.getNumber fd)
        wt (wire/wire-type tn)
        opts (scalar-opts fd)
        packable? (not (#{"STRING" "BYTES"} tn))]
    (if (.isRepeated fd)
      (cond-> [[(wire/tag n wt) (wire/scalar-reader tn :expanded opts)]]
        packable? (conj [(wire/tag n WireFormat/WIRETYPE_LENGTH_DELIMITED) (wire/scalar-reader tn :packed opts)]))
      (let [r (wire/scalar-reader tn :singular opts)]
        [[(wire/tag n wt) (if (.-has-presence? f) r (normalized-reader r (.-default f)))]]))))

(defn- compile-field
  "Everything about one field, given a way to reach nested types."
  [^Descriptors$FieldDescriptor fd real-oneof-index nested-of]
  (let [kind (kind-of fd)
        map-field (.isMapField fd)
        repeated (and (.isRepeated fd) (not map-field))
        grp (group? fd)
        n (.getNumber fd)
        tn (type-name fd)
        mode (if (.isRepeated fd) (if (.isPacked fd) :packed :expanded) :singular)
        entry (when map-field (.getMessageType fd))
        key-fd (when entry (.findFieldByName entry "key"))
        val-fd (when entry (.findFieldByName entry "value"))
        writer (cond
                 map-field (wire/map-writer* n (type-name key-fd) (type-name val-fd))
                 (= kind :message) (wire/message-writer n mode grp)
                 :else (wire/scalar-writer tn n mode))]
    (map->CompiledField
     {:fd fd
      :slot (long (.getIndex fd))
      :number (long n)
      :kind kind
      :repeated? repeated
      :map? map-field
      :group? grp
      :has-presence? (.hasPresence fd)
      :required? (.isRequired fd)
      :default (when-not map-field (slot-default fd))
      :oneof (long (if-let [oo (.getRealContainingOneof fd)] (real-oneof-index oo) -1))
      :writer writer
      :enum-type (when (= kind :enum) (.getEnumType fd))
      :key-type (when map-field (type-name key-fd))
      :val-type (when map-field (type-name val-fd))
      :val-enum-type (when (and map-field (= :enum (kind-of val-fd))) (.getEnumType val-fd))
      :nested (cond
                map-field (when (= :message (kind-of val-fd)) (nested-of (.getMessageType val-fd)))
                (= kind :message) (nested-of (.getMessageType fd)))})))

(defn- field-readers
  "[[tag reader] ...] for one compiled field."
  [^CompiledField f nested-parser]
  (let [^Descriptors$FieldDescriptor fd (.-fd f)
        n (.-number f)]
    (cond
      (.-map? f)
      (let [entry (.getMessageType fd)
            key-fd (.findFieldByName entry "key")
            val-fd (.findFieldByName entry "value")]
        [[(wire/tag n WireFormat/WIRETYPE_LENGTH_DELIMITED)
          (wire/map-reader* (.-key-type f) (scalar-opts key-fd) (slot-default key-fd)
                            (.-val-type f)
                            (if (= :message (kind-of val-fd))
                              {:parser (nested-parser f)}
                              (scalar-opts val-fd))
                            (if (= :message (kind-of val-fd))
                              ;; the value type's default instance: what its
                              ;; parser makes of no bytes at all
                              (let [p (nested-parser f)]
                                (delay (.parseFrom ^Parser (deref p) ByteString/EMPTY)))
                              (slot-default val-fd)))]])

      (= :message (.-kind f))
      (let [mode (if (.-repeated? f) :expanded :singular)]
        [[(wire/tag n (if (.-group? f) WireFormat/WIRETYPE_START_GROUP WireFormat/WIRETYPE_LENGTH_DELIMITED))
          (wire/message-reader* n mode (.-group? f) (nested-parser f))]])

      :else (scalar-readers fd f))))

;; ---------------------------------------------------------------------------
;; The type

(def ^:private dense-limit 4096)

(defn- compile-type
  "nested-of: Descriptor -> IDeref of its CompiledType. parser-of:
  IDeref-of-CompiledType -> IDeref of its Parser."
  ^CompiledType [^Descriptors$Descriptor d nested-of parser-of]
  (let [oneofs (.getRealOneofs d)
        oneof-index (into {} (map-indexed (fn [i oo] [oo i])) oneofs)
        fields (mapv #(compile-field % oneof-index nested-of) (.getFields d))
        by-slot (object-array (count fields))
        _ (doseq [^CompiledField f fields] (aset by-slot (.-slot f) f))
        writers (object-array (sort-by :number fields))
        entries (into []
                      (mapcat (fn [^CompiledField f]
                                (map (fn [[tag r]] [tag (.-slot f) r (.-oneof f)])
                                     (field-readers f (fn [^CompiledField f] (parser-of (.-nested f)))))))
                      fields)
        tags (mapv first entries)
        dense? (and (seq tags) (every? #(< -1 % dense-limit) tags))
        size (if dense? (inc (long (apply max tags))) (count entries))
        sorted (if dense? entries (sort-by first entries))
        tag-slots (int-array size -1)
        tag-readers (object-array size)
        tag-oneofs (int-array size -1)]
    (doseq [[i [tag slot r oneof]] (map-indexed vector sorted)
            :let [pos (if dense? (long tag) (long i))]]
      (aset tag-slots pos (int slot))
      (aset tag-readers pos r)
      (aset tag-oneofs pos (int oneof)))
    (map->CompiledType
     {:descriptor d
      :fields by-slot
      :writers writers
      :tags (when-not dense? (int-array (map first sorted)))
      :tag-slots tag-slots
      :tag-readers tag-readers
      :tag-oneofs tag-oneofs
      :dense (long (if dense? size 0))
      :oneof-slots (object-array
                    (map (fn [^Descriptors$OneofDescriptor oo]
                           (int-array (map (fn [^Descriptors$FieldDescriptor fd] (.getIndex fd)) (.getFields oo))))
                         oneofs))
      :required-slots (int-array (keep (fn [^CompiledField f] (when (.-required? f) (.-slot f))) fields))
      :needs-unknown? (boolean (some (fn [^CompiledField f]
                                       (or (some-> ^Descriptors$EnumDescriptor (.-enum-type f) .isClosed)
                                           (some-> ^Descriptors$EnumDescriptor (.-val-enum-type f) .isClosed)))
                                     fields))})))

(defn compiler
  "A compile function with its own cache, keyed by Descriptor identity.
  parser-of takes an IDeref of a CompiledType and returns an IDeref of the
  Parser that reads that type — the message layer's, or DynamicMessage's in
  tests. Nested types compile on first use, which is what makes cyclic
  descriptors terminate."
  [parser-of]
  (let [cache (ConcurrentHashMap.)
        lock (Object.)]
    (fn compile [^Descriptors$Descriptor d]
      (or (.get cache d)
          (locking lock
            (or (.get cache d)
                (let [t (compile-type d (fn [nd] (delay (compile nd))) parser-of)]
                  (.put cache d t)
                  t)))))))

;; ---------------------------------------------------------------------------
;; The loops

(defn- reader-position
  "Index into the tag table for this tag, or -1."
  ^long [^CompiledType t ^long tag]
  (let [dense (.-dense t)]
    (if (pos? dense)
      (if (and (>= tag 0) (< tag dense) (>= (aget ^ints (.-tag-slots t) tag) 0)) tag -1)
      (let [i (Arrays/binarySearch ^ints (.-tags t) (int tag))]
        (if (>= i 0) i -1)))))

(defn read-into
  "Parse `in` into `slots` until end of input or an end-group tag, which is
  left for the caller (CodedInputStream.readGroup checks it). Returns the
  UnknownFieldSet.Builder if anything went unknown, else nil."
  ^UnknownFieldSet$Builder [^CompiledType t ^CodedInputStream in ^objects slots]
  (let [^ints tag-slots (.-tag-slots t)
        ^objects tag-readers (.-tag-readers t)
        ^ints tag-oneofs (.-tag-oneofs t)
        ^objects oneof-slots (.-oneof-slots t)]
    (loop [unknown (when (.-needs-unknown? t) (UnknownFieldSet/newBuilder))]
      (let [tag (.readTag in)]
        (cond
          (zero? tag) unknown
          (= WireFormat/WIRETYPE_END_GROUP (WireFormat/getTagWireType tag)) unknown
          :else
          (let [pos (reader-position t tag)]
            (if (neg? pos)
              (let [^UnknownFieldSet$Builder u (or unknown (UnknownFieldSet/newBuilder))]
                (.mergeFieldFrom u tag in)
                (recur u))
              (let [slot (aget tag-slots pos)
                    ^FieldReader r (aget tag-readers pos)
                    oneof (aget tag-oneofs pos)]
                (aset slots slot (.read r in (aget slots slot) unknown))
                (when (>= oneof 0)
                  ;; last member seen on the wire wins, as in every parser
                  (let [^ints siblings (aget oneof-slots oneof)
                        c (alength siblings)]
                    (loop [i 0]
                      (when (< i c)
                        (let [s (aget siblings i)]
                          (when (not= s slot) (aset slots s nil)))
                        (recur (unchecked-inc i))))))
                (recur unknown)))))))))

(defn- present?
  "Non-nil, and for collections non-empty: what reaches the wire."
  [^CompiledField f v]
  (and (some? v)
       (cond
         (.-repeated? f) (not (.isEmpty ^List v))
         (.-map? f) (not (.isEmpty ^Map v))
         :else true)))

(defn write-slots
  "Write present fields in field-number order, then unknown fields."
  [^CompiledType t ^objects slots ^UnknownFieldSet unknown ^CodedOutputStream out]
  (let [^objects writers (.-writers t)
        c (alength writers)]
    (loop [i 0]
      (when (< i c)
        (let [^CompiledField f (aget writers i)
              v (aget slots (.-slot f))]
          (when (present? f v)
            (.write ^FieldWriter (.-writer f) out v))
          (recur (unchecked-inc i)))))
    (when unknown (.writeTo unknown out))))

(defn size-slots
  "The serialized size of these slots and unknown fields."
  ^long [^CompiledType t ^objects slots ^UnknownFieldSet unknown]
  (let [^objects writers (.-writers t)
        c (alength writers)]
    (loop [i 0 acc 0]
      (if (< i c)
        (let [^CompiledField f (aget writers i)
              v (aget slots (.-slot f))]
          (recur (unchecked-inc i)
                 (if (present? f v)
                   (unchecked-add acc (.size ^FieldWriter (.-writer f) v))
                   acc)))
        (if unknown (unchecked-add acc (.getSerializedSize unknown)) acc)))))

(defn field-count ^long [^CompiledType t] (alength ^objects (.-fields t)))
