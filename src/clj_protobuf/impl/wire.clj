(ns clj-protobuf.impl.wire
  "Wire primitives for the compiled codec: one writer and one reader per
  field, built once from the field's type and number, over protobuf-java's
  own CodedOutputStream and CodedInputStream. Varints, zigzag, fixed widths,
  UTF-8 and the length arithmetic stay protobuf-java's code; what this
  namespace adds is the choice of which call to make, made once per field
  instead of once per value.

  A writer writes one field — tag included, every element for repeateds, one
  length-delimited run when packed — and reports its serialized size. A
  reader is handed the input positioned just after this field's tag and the
  slot's current value, and returns the slot's new value: the scalar, the
  list with one more element, the map with one more entry, or a merged
  message when a singular message field repeats on the wire. Readers are
  keyed by tag rather than by field number upstream, because a repeated
  scalar may arrive packed or expanded whatever its descriptor says it
  writes, and the two are different tags.

  Slot representations, shared with the compiler and the codec:
    int32 kinds  Integer      int64 kinds  Long
    float        Float        double       Double
    bool         Boolean      string       String
    bytes        ByteString   enum         Integer (the number)
    message      Message      repeated     java.util.ArrayList
    map          java.util.LinkedHashMap (insertion order is wire order)"
  (:import [com.google.protobuf
            ByteString
            CodedInputStream
            CodedOutputStream
            ExtensionRegistryLite
            Message
            MessageLite
            Parser
            UnknownFieldSet$Builder
            WireFormat]
           [clojure.lang IDeref]
           [java.util ArrayList LinkedHashMap List Map Map$Entry]))

(set! *warn-on-reflection* true)

(definterface FieldWriter
  (^void write [^com.google.protobuf.CodedOutputStream out ^Object v])
  (^int size [^Object v]))

(definterface FieldReader
  (^Object read [^com.google.protobuf.CodedInputStream in
                 ^Object current
                 ^com.google.protobuf.UnknownFieldSet$Builder unknown]))

;; Entry points for code outside this namespace's reload group — tests on
;; the plain-clj leg in particular, where the reflection gate reloads these
;; namespaces in the same JVM and a caller hinted on the old interface class
;; would meet instances of the new one.
(defn write! [^FieldWriter w ^CodedOutputStream out v] (.write w out v))
(defn size-of ^long [^FieldWriter w v] (.size w v))
(defn read! [^FieldReader r ^CodedInputStream in current ^UnknownFieldSet$Builder unknown]
  (.read r in current unknown))

;; ---------------------------------------------------------------------------
;; Scalars

;; One macro expansion per scalar type: the three writer shapes and the two
;; reader shapes, over the uniformly named CodedOutputStream/CodedInputStream
;; methods (writeX, writeXNoTag, computeXSize, computeXSizeNoTag, readX).
(defmacro ^:private defscalar
  [suffix coerce]
  (let [write      (symbol (str "write" suffix))
        write-nt   (symbol (str "write" suffix "NoTag"))
        size       (symbol (str "compute" suffix "Size"))
        size-nt    (symbol (str "compute" suffix "SizeNoTag"))
        read       (symbol (str "read" suffix))
        singular   (symbol (str (name suffix) "-singular"))
        expanded   (symbol (str (name suffix) "-expanded"))
        packed     (symbol (str (name suffix) "-packed"))
        reader     (symbol (str (name suffix) "-reader"))
        packed-rd  (symbol (str (name suffix) "-packed-reader"))]
    `(do
       (defn- ~singular ^FieldWriter [~'n]
         (let [~'n (int ~'n)]
           (reify FieldWriter
             (write [~'_ ~'out ~'v] (. ~'out ~write ~'n (~coerce ~'v)))
             (size [~'_ ~'v] (. CodedOutputStream ~size ~'n (~coerce ~'v))))))
       (defn- ~expanded ^FieldWriter [~'n]
         (let [~'n (int ~'n)]
           (reify FieldWriter
             (write [~'_ ~'out ~'v]
               (let [~'vs ~(with-meta 'v {:tag 'java.util.List})
                     ~'c (.size ~'vs)]
                 (loop [~'i 0]
                   (when (< ~'i ~'c)
                     (. ~'out ~write ~'n (~coerce (.get ~'vs ~'i)))
                     (recur (unchecked-inc ~'i))))))
             (size [~'_ ~'v]
               (let [~'vs ~(with-meta 'v {:tag 'java.util.List})
                     ~'c (.size ~'vs)]
                 (loop [~'i 0 ~'acc 0]
                   (if (< ~'i ~'c)
                     (recur (unchecked-inc ~'i)
                            (unchecked-add-int ~'acc (. CodedOutputStream ~size ~'n (~coerce (.get ~'vs ~'i)))))
                     ~'acc)))))))
       (defn- ~packed ^FieldWriter [~'n]
         (let [~'n (int ~'n)
               ~'tag-size (CodedOutputStream/computeTagSize ~'n)
               ~'data-size (fn [~(with-meta 'vs {:tag 'java.util.List})]
                             (let [~'c (.size ~'vs)]
                               (loop [~'i 0 ~'acc 0]
                                 (if (< ~'i ~'c)
                                   (recur (unchecked-inc ~'i)
                                          (unchecked-add-int ~'acc (. CodedOutputStream ~size-nt (~coerce (.get ~'vs ~'i)))))
                                   ~'acc))))]
           (reify FieldWriter
             (write [~'_ ~'out ~'v]
               (let [~'vs ~(with-meta 'v {:tag 'java.util.List})
                     ~'c (.size ~'vs)]
                 (.writeTag ~'out ~'n WireFormat/WIRETYPE_LENGTH_DELIMITED)
                 (.writeUInt32NoTag ~'out (int (~'data-size ~'vs)))
                 (loop [~'i 0]
                   (when (< ~'i ~'c)
                     (. ~'out ~write-nt (~coerce (.get ~'vs ~'i)))
                     (recur (unchecked-inc ~'i))))))
             (size [~'_ ~'v]
               (let [~'d (int (~'data-size ~(with-meta 'v {:tag 'java.util.List})))]
                 (unchecked-add-int ~'tag-size
                                    (unchecked-add-int (CodedOutputStream/computeUInt32SizeNoTag ~'d) ~'d)))))))
       (defn- ~reader ^FieldReader []
         (reify FieldReader
           (read [~'_ ~'in ~'_ ~'_] (. ~'in ~read))))
       (defn- ~packed-rd ^FieldReader []
         (reify FieldReader
           (read [~'_ ~'in ~'current ~'_]
             (let [~(with-meta 'l {:tag 'java.util.ArrayList}) (or ~'current (ArrayList.))
                   ~'len (.readRawVarint32 ~'in)
                   ~'limit (.pushLimit ~'in ~'len)]
               (while (pos? (.getBytesUntilLimit ~'in))
                 (.add ~'l (. ~'in ~read)))
               (.popLimit ~'in ~'limit)
               ~'l)))))))

(defscalar Int32 int)
(defscalar Int64 long)
(defscalar UInt32 int)
(defscalar UInt64 long)
(defscalar SInt32 int)
(defscalar SInt64 long)
(defscalar Fixed32 int)
(defscalar Fixed64 long)
(defscalar SFixed32 int)
(defscalar SFixed64 long)
(defscalar Float float)
(defscalar Double double)
(defscalar Bool boolean)
(defscalar Enum int)

;; Strings and bytes: never packed, and the string reader has two flavours.
(defn- string-singular ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v] (.writeString out n ^String v))
      (size [_ v] (CodedOutputStream/computeStringSize n ^String v)))))

(defn- string-expanded ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v]
        (let [^List vs v c (.size vs)]
          (loop [i 0]
            (when (< i c)
              (.writeString out n ^String (.get vs i))
              (recur (unchecked-inc i))))))
      (size [_ v]
        (let [^List vs v c (.size vs)]
          (loop [i 0 acc 0]
            (if (< i c)
              (recur (unchecked-inc i)
                     (unchecked-add-int acc (CodedOutputStream/computeStringSize n ^String (.get vs i))))
              acc)))))))

(defn- string-reader
  "utf8? selects readStringRequireUtf8: malformed input is a parse error,
  as the descriptor's utf8_validation feature decided."
  ^FieldReader [utf8?]
  (if utf8?
    (reify FieldReader (read [_ in _ _] (.readStringRequireUtf8 in)))
    (reify FieldReader (read [_ in _ _] (.readString in)))))

(defn- bytes-singular ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v] (.writeBytes out n ^ByteString v))
      (size [_ v] (CodedOutputStream/computeBytesSize n ^ByteString v)))))

(defn- bytes-expanded ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v]
        (let [^List vs v c (.size vs)]
          (loop [i 0]
            (when (< i c)
              (.writeBytes out n ^ByteString (.get vs i))
              (recur (unchecked-inc i))))))
      (size [_ v]
        (let [^List vs v c (.size vs)]
          (loop [i 0 acc 0]
            (if (< i c)
              (recur (unchecked-inc i)
                     (unchecked-add-int acc (CodedOutputStream/computeBytesSize n ^ByteString (.get vs i))))
              acc)))))))

(defn- bytes-reader ^FieldReader []
  (reify FieldReader (read [_ in _ _] (.readBytes in))))

;; Closed enums: a number with no declared value is not a value of the
;; field — it goes to unknown fields, and the slot is left as it was.
(defn- closed-enum-reader
  ^FieldReader [n known?]
  (let [n (int n)]
    (reify FieldReader
      (read [_ in current unknown]
        (let [x (.readEnum in)]
          (if (known? x)
            (Integer/valueOf x)
            (do (.mergeVarintField unknown n x) current)))))))

(defn- closed-enum-expanded-reader
  ^FieldReader [n known?]
  (let [n (int n)]
    (reify FieldReader
      (read [_ in current unknown]
        (let [^ArrayList l (or current (ArrayList.))
              x (.readEnum in)]
          (if (known? x)
            (.add l (Integer/valueOf x))
            (.mergeVarintField unknown n x))
          l)))))

(defn- closed-enum-packed-reader
  ^FieldReader [n known?]
  (let [n (int n)]
    (reify FieldReader
      (read [_ in current unknown]
        (let [^ArrayList l (or current (ArrayList.))
              len (.readRawVarint32 in)
              limit (.pushLimit in len)]
          (while (pos? (.getBytesUntilLimit in))
            (let [x (.readEnum in)]
              (if (known? x)
                (.add l (Integer/valueOf x))
                (.mergeVarintField unknown n x))))
          (.popLimit in limit)
          l)))))

;; Every repeated scalar reader appends one element in expanded form. The
;; scalar readers above return the element; this wraps one into the list.
(defn- expanded-reader ^FieldReader [^FieldReader element]
  (reify FieldReader
    (read [_ in current unknown]
      (let [^ArrayList l (or current (ArrayList.))]
        (.add l (.read element in nil unknown))
        l))))

;; ---------------------------------------------------------------------------
;; Messages and groups

(def ^:private ^ExtensionRegistryLite no-extensions (ExtensionRegistryLite/getEmptyRegistry))

(defn- message-singular ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v] (.writeMessage out n ^MessageLite v))
      (size [_ v] (CodedOutputStream/computeMessageSize n ^MessageLite v)))))

(defn- message-expanded ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v]
        (let [^List vs v c (.size vs)]
          (loop [i 0]
            (when (< i c)
              (.writeMessage out n ^MessageLite (.get vs i))
              (recur (unchecked-inc i))))))
      (size [_ v]
        (let [^List vs v c (.size vs)]
          (loop [i 0 acc 0]
            (if (< i c)
              (recur (unchecked-inc i)
                     (unchecked-add-int acc (CodedOutputStream/computeMessageSize n ^MessageLite (.get vs i))))
              acc)))))))

(defn- group-singular ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v] (.writeGroup out n ^MessageLite v))
      (size [_ v] (CodedOutputStream/computeGroupSize n ^MessageLite v)))))

(defn- group-expanded ^FieldWriter [n]
  (let [n (int n)]
    (reify FieldWriter
      (write [_ out v]
        (let [^List vs v c (.size vs)]
          (loop [i 0]
            (when (< i c)
              (.writeGroup out n ^MessageLite (.get vs i))
              (recur (unchecked-inc i))))))
      (size [_ v]
        (let [^List vs v c (.size vs)]
          (loop [i 0 acc 0]
            (if (< i c)
              (recur (unchecked-inc i)
                     (unchecked-add-int acc (CodedOutputStream/computeGroupSize n ^MessageLite (.get vs i))))
              acc)))))))

(defn- merge-into
  "A singular message field repeated on the wire merges, as every protobuf
  parser does; the second occurrence's fields win, repeateds concatenate."
  ^Message [current ^Message incoming]
  (if (nil? current)
    incoming
    (-> (.toBuilder ^Message current)
        (.mergeFrom incoming)
        (.buildPartial))))

(defn- message-reader
  "parser is an IDeref of the nested type's Parser: descriptors can be
  cyclic, so the nested parser may not exist when this reader is built."
  ^FieldReader [^IDeref parser]
  (reify FieldReader
    (read [_ in current _]
      (merge-into current (.readMessage in ^Parser (.deref parser) no-extensions)))))

(defn- message-expanded-reader ^FieldReader [^IDeref parser]
  (reify FieldReader
    (read [_ in current _]
      (let [^ArrayList l (or current (ArrayList.))]
        (.add l (.readMessage in ^Parser (.deref parser) no-extensions))
        l))))

(defn- group-reader ^FieldReader [n ^IDeref parser]
  (let [n (int n)]
    (reify FieldReader
      (read [_ in current _]
        (merge-into current (.readGroup in n ^Parser (.deref parser) no-extensions))))))

(defn- group-expanded-reader ^FieldReader [n ^IDeref parser]
  (let [n (int n)]
    (reify FieldReader
      (read [_ in current _]
        (let [^ArrayList l (or current (ArrayList.))]
          (.add l (.readGroup in n ^Parser (.deref parser) no-extensions))
          l)))))

;; ---------------------------------------------------------------------------
;; Maps: one length-delimited entry per pair, key as field 1 and value as
;; field 2, both always written — which is what protoc's MapEntry does.

(defn- map-writer
  ^FieldWriter [n ^FieldWriter key-writer ^FieldWriter val-writer]
  (let [n (int n)
        tag-size (CodedOutputStream/computeTagSize n)]
    (reify FieldWriter
      (write [_ out v]
        (let [it (.iterator (.entrySet ^Map v))]
          (while (.hasNext it)
            (let [^Map$Entry e (.next it)
                  k (.getKey e)
                  x (.getValue e)
                  entry (unchecked-add-int (.size key-writer k) (.size val-writer x))]
              (.writeTag out n WireFormat/WIRETYPE_LENGTH_DELIMITED)
              (.writeUInt32NoTag out entry)
              (.write key-writer out k)
              (.write val-writer out x)))))
      (size [_ v]
        (let [it (.iterator (.entrySet ^Map v))]
          (loop [acc 0]
            (if (.hasNext it)
              (let [^Map$Entry e (.next it)
                    entry (unchecked-add-int (.size key-writer (.getKey e)) (.size val-writer (.getValue e)))]
                (recur (unchecked-add-int acc
                                          (unchecked-add-int tag-size
                                                             (unchecked-add-int (CodedOutputStream/computeUInt32SizeNoTag entry) entry)))))
              acc)))))))

(defn- resolve-default
  "A default may be lazy (an IDeref): a message-valued map's default is the
  value type's default instance, which may not exist when the reader is
  built. Descriptors are cyclic."
  [d]
  (if (instance? IDeref d) (.deref ^IDeref d) d))

(defn- map-reader
  "Reads one entry into the map. key-tag and val-tag are the tags the entry's
  key and value carry (numbers 1 and 2 with their wire types); anything else
  inside an entry is skipped, and a missing key or value takes its default,
  as protoc's parsers do."
  ^FieldReader [key-tag ^FieldReader key-reader key-default
                val-tag ^FieldReader val-reader val-default]
  (let [key-tag (int key-tag) val-tag (int val-tag)]
    (reify FieldReader
      (read [_ in current unknown]
        (let [^LinkedHashMap m (or current (LinkedHashMap.))
              len (.readRawVarint32 in)
              limit (.pushLimit in len)]
          (loop [k nil x nil]
            (let [tag (.readTag in)]
              (cond
                (zero? tag) (do (.popLimit in limit)
                                (.put m
                                      (if (nil? k) (resolve-default key-default) k)
                                      (if (nil? x) (resolve-default val-default) x))
                                m)
                (= tag key-tag) (recur (.read key-reader in nil unknown) x)
                (= tag val-tag) (recur k (.read val-reader in x unknown))
                :else (do (.skipField in tag) (recur k x))))))))))

;; ---------------------------------------------------------------------------
;; Construction by type name

(defn wire-type
  "The wire type a field's type serializes with, as WireFormat's constant."
  ^long [^String type-name]
  (case type-name
    ("INT32" "INT64" "UINT32" "UINT64" "SINT32" "SINT64" "BOOL" "ENUM") WireFormat/WIRETYPE_VARINT
    ("FIXED64" "SFIXED64" "DOUBLE") WireFormat/WIRETYPE_FIXED64
    ("STRING" "BYTES" "MESSAGE") WireFormat/WIRETYPE_LENGTH_DELIMITED
    "GROUP" WireFormat/WIRETYPE_START_GROUP
    ("FIXED32" "SFIXED32" "FLOAT") WireFormat/WIRETYPE_FIXED32))

(defn tag
  "WireFormat.makeTag, which is package-private — with the int overflow
  readTag has: the largest field number's tags are negative, and a table
  keyed by tag must key them the way they arrive."
  ^long [^long field-number ^long wire-type]
  (long (unchecked-int (bit-or (bit-shift-left field-number 3) wire-type))))

(defn scalar-writer
  "A writer for a scalar (non-message) field of this type name and number.
  mode is :singular, :expanded (one tag per element) or :packed."
  ^FieldWriter [^String type-name n mode]
  (case type-name
    "INT32"    (case mode :singular (Int32-singular n) :expanded (Int32-expanded n) :packed (Int32-packed n))
    "INT64"    (case mode :singular (Int64-singular n) :expanded (Int64-expanded n) :packed (Int64-packed n))
    "UINT32"   (case mode :singular (UInt32-singular n) :expanded (UInt32-expanded n) :packed (UInt32-packed n))
    "UINT64"   (case mode :singular (UInt64-singular n) :expanded (UInt64-expanded n) :packed (UInt64-packed n))
    "SINT32"   (case mode :singular (SInt32-singular n) :expanded (SInt32-expanded n) :packed (SInt32-packed n))
    "SINT64"   (case mode :singular (SInt64-singular n) :expanded (SInt64-expanded n) :packed (SInt64-packed n))
    "FIXED32"  (case mode :singular (Fixed32-singular n) :expanded (Fixed32-expanded n) :packed (Fixed32-packed n))
    "FIXED64"  (case mode :singular (Fixed64-singular n) :expanded (Fixed64-expanded n) :packed (Fixed64-packed n))
    "SFIXED32" (case mode :singular (SFixed32-singular n) :expanded (SFixed32-expanded n) :packed (SFixed32-packed n))
    "SFIXED64" (case mode :singular (SFixed64-singular n) :expanded (SFixed64-expanded n) :packed (SFixed64-packed n))
    "FLOAT"    (case mode :singular (Float-singular n) :expanded (Float-expanded n) :packed (Float-packed n))
    "DOUBLE"   (case mode :singular (Double-singular n) :expanded (Double-expanded n) :packed (Double-packed n))
    "BOOL"     (case mode :singular (Bool-singular n) :expanded (Bool-expanded n) :packed (Bool-packed n))
    "ENUM"     (case mode :singular (Enum-singular n) :expanded (Enum-expanded n) :packed (Enum-packed n))
    "STRING"   (case mode :singular (string-singular n) :expanded (string-expanded n))
    "BYTES"    (case mode :singular (bytes-singular n) :expanded (bytes-expanded n))))

(defn scalar-reader
  "A reader for a scalar field of this type name. mode is :singular,
  :expanded or :packed. opts: :utf8? for strings; for closed enums :known?
  (a predicate on the number) and :field-number, so undeclared numbers can
  be routed to unknown fields."
  ^FieldReader [^String type-name mode {:keys [utf8? known? field-number]}]
  (let [element (case type-name
                  "INT32" (Int32-reader) "INT64" (Int64-reader)
                  "UINT32" (UInt32-reader) "UINT64" (UInt64-reader)
                  "SINT32" (SInt32-reader) "SINT64" (SInt64-reader)
                  "FIXED32" (Fixed32-reader) "FIXED64" (Fixed64-reader)
                  "SFIXED32" (SFixed32-reader) "SFIXED64" (SFixed64-reader)
                  "FLOAT" (Float-reader) "DOUBLE" (Double-reader)
                  "BOOL" (Bool-reader)
                  "ENUM" (if known? (closed-enum-reader field-number known?) (Enum-reader))
                  "STRING" (string-reader utf8?)
                  "BYTES" (bytes-reader))]
    (case mode
      :singular element
      :expanded (if (and (= type-name "ENUM") known?)
                  (closed-enum-expanded-reader field-number known?)
                  (expanded-reader element))
      :packed (case type-name
                "INT32" (Int32-packed-reader) "INT64" (Int64-packed-reader)
                "UINT32" (UInt32-packed-reader) "UINT64" (UInt64-packed-reader)
                "SINT32" (SInt32-packed-reader) "SINT64" (SInt64-packed-reader)
                "FIXED32" (Fixed32-packed-reader) "FIXED64" (Fixed64-packed-reader)
                "SFIXED32" (SFixed32-packed-reader) "SFIXED64" (SFixed64-packed-reader)
                "FLOAT" (Float-packed-reader) "DOUBLE" (Double-packed-reader)
                "BOOL" (Bool-packed-reader)
                "ENUM" (if known? (closed-enum-packed-reader field-number known?) (Enum-packed-reader))))))

(defn message-writer
  "A writer for a message- or group-typed field. group? selects start/end
  group tags (proto2 groups, editions DELIMITED)."
  ^FieldWriter [n mode group?]
  (if group?
    (case mode :singular (group-singular n) :expanded (group-expanded n))
    (case mode :singular (message-singular n) :expanded (message-expanded n))))

(defn message-reader*
  "A reader for a message- or group-typed field; parser is an IDeref of the
  nested type's Parser."
  ^FieldReader [n mode group? ^IDeref parser]
  (if group?
    (case mode :singular (group-reader n parser) :expanded (group-expanded-reader n parser))
    (case mode :singular (message-reader parser) :expanded (message-expanded-reader parser))))

(defn map-writer*
  "A writer for a map field: key-type and val-type are type names, the value
  side may be a message (val-parser nil then) — messages in maps are always
  length-delimited."
  ^FieldWriter [n ^String key-type ^String val-type]
  (map-writer n
              (scalar-writer key-type 1 :singular)
              (if (= val-type "MESSAGE")
                (message-singular 2)
                (scalar-writer val-type 2 :singular))))

(defn map-reader*
  "A reader for a map field. val-opts are the value's scalar-reader opts,
  or {:parser IDeref} for message values. Defaults fill a missing key or
  value: key-default and val-default in slot representation, either of
  which may be an IDeref resolved on first use."
  ^FieldReader [^String key-type key-opts key-default
                ^String val-type val-opts val-default]
  (map-reader (tag 1 (wire-type key-type))
              (scalar-reader key-type :singular key-opts)
              key-default
              (tag 2 (wire-type val-type))
              (if (= val-type "MESSAGE")
                (message-reader (:parser val-opts))
                (scalar-reader val-type :singular val-opts))
              val-default))
