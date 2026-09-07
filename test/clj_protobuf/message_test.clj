(ns clj-protobuf.message-test
  "The compiled message against DynamicMessage: same bytes, equal and
  equally hashed, same TextFormat, the same reflective API in both
  directions, the same merge semantics, the same parser overloads, the same
  initialization failures — and the exact call pattern grpc-java's
  marshaller uses, which is the first thing to break if size and bytes ever
  disagree.

  DynamicMessage prototypes come from the same file-descriptor vars the
  compiled ones do, so both live in one descriptor pool and protobuf-java's
  own equality rule (descriptor identity) allows them to be equal. No
  generated Java classes: this runs on the plain-clj leg, and it does not
  type-hint the message layer's types, for the reason wire_test gives."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clj-protobuf.core :as pb]
            [clj-protobuf.impl.message :as message]
            [clj-protobuf.runtime :as rt]
            [fixtures.e2024.kitchen :as kitchen]
            [fixtures.nested.nested :as nested]
            [fixtures.wire.e2024 :as we]
            [fixtures.wire.p2 :as wp2]
            [fixtures.wire.p3 :as wp3])
  (:import [com.google.protobuf
            ByteString
            CodedInputStream
            CodedOutputStream
            Descriptors$Descriptor
            Descriptors$EnumValueDescriptor
            Descriptors$FieldDescriptor
            DynamicMessage
            ExtensionRegistryLite
            InvalidProtocolBufferException
            Message
            Message$Builder
            Parser
            TextFormat
            UninitializedMessageException
            UnknownFieldSet]
           [java.io ByteArrayInputStream ByteArrayOutputStream OutputStream]
           [java.nio ByteBuffer]))

;; ---------------------------------------------------------------------------
;; Two arms over one pool

(defn- desc ^Descriptors$Descriptor [^Message proto] (.getDescriptorForType proto))
(defn- fd ^Descriptors$FieldDescriptor [^Descriptors$Descriptor d ^String n]
  (or (.findFieldByName d (str/replace n "-" "_")) (throw (ex-info (str "no field " n) {}))))

(defn- compiled ^Message [^Message proto] (message/prototype (desc proto)))
(defn- dynamic ^Message [^Message proto] (DynamicMessage/getDefaultInstance (desc proto)))

(defn- parse-with ^Message [^Message proto ^bytes bs]
  (.parseFrom ^Parser (.getParserForType proto) bs))

(def ^:private values
  ;; the compile suite's corpus, through the generated (dynamic-arm) fns
  [["wire.p2/Wire" wp2/Wire-prototype
    (wp2/Wire->proto {:i32 -1 :i64 -2 :u32 3 :u64 4 :s32 -5 :s64 -6 :f32 7 :f64 8 :sf32 -9 :sf64 -10
                      :flt (float 1.5) :dbl 2.5 :flag true :str "héllo 日本 😀" :raw (byte-array [0 127 -128])
                      :color :CLOSED_B :leaf {:id "leaf"}
                      :unpacked [1 -2 300] :packed [4 5] :packed-s64 [-1 Long/MAX_VALUE] :packed-dbl [0.5 -0.0]
                      :colors [:CLOSED_A :CLOSED_B] :leaves [{:id "a"} {:id "b"}] :names ["x" "" "日本"]
                      :by-id {2 {:id "two"} -1 {:id "minus"}} :by-color {"a" :CLOSED_A "b" :CLOSED_UNSPECIFIED}
                      :pick-leaf {:id "picked"} :dflt-int 42 :dflt-str "other" :dflt-enum :CLOSED_A
                      :grp {:note "g"} :grps [{:n 1} {:n 2}] :high "high" :huge 7})]
   ["wire.p3/Wire" wp3/Wire-prototype
    (wp3/Wire->proto {:i32 -1 :flt (float 1.5) :str "s" :raw (byte-array [1 2]) :color :OPEN_B :leaf {:id "leaf"}
                      :packed [1 -2 300] :unpacked [4 5] :colors [:OPEN_A :OPEN_B] :leaves [{:id "a"}] :names ["x"]
                      :by-id {1 {:id "one"}} :by-color {"k" :OPEN_A} :pick-i64 99 :opt-i32 0 :opt-str "" :huge 1})]
   ["wire.e2024/Wire" we/Wire-prototype
    (we/Wire->proto {:str "s" :checked "c" :expanded [1 2] :packed [3 4] :delimited {:id "d"}
                     :length-prefixed {:id "l"} :implicit 5 :explicit 0 :open :OPEN_A :closed :CLOSED_B
                     :closed-list [:CLOSED_A]})]
   ["e2024/Kitchen" kitchen/Kitchen-prototype
    (kitchen/Kitchen->proto {:str-field "hello" :int-field 42 :bool-field true :bytes-field (byte-array [1 2 3])
                             :dbl-field 3.5 :long-field 9007199254740993 :enum-field :COLOR_RED
                             :msg-field {:id "nested"} :tags ["a" "b"] :children [{:id "c1"} {:id "c2"}]
                             :counts {"x" 1 "y" 2} :choice-int 7 :ts {:seconds 5 :nanos 100} :dur {:seconds 30}
                             :wrapped {:value "wrapped"} :implicit-field "imp" :delimited {:note "grouped"}})]
   ["nested/Outer" nested/Outer-prototype
    (nested/Outer->proto {:id "o" :counts {"k" 1} :inner {:name "i" :labels {"env" "prod"} :innermost {:depth 3}}})]])

(deftest parsed-messages-are-equal-to-dynamic-message-in-every-way
  (doseq [[name proto ^Message dyn] values]
    (testing name
      (let [bs (.toByteArray dyn)
            cm (parse-with (compiled proto) bs)]
        (is (not (instance? DynamicMessage cm)))
        (is (java.util.Arrays/equals bs (.toByteArray cm)) "bytes")
        (is (= (alength bs) (.getSerializedSize cm)) "size")
        (is (.equals cm dyn) "compiled equals dynamic")
        (is (.equals dyn cm) "dynamic equals compiled")
        (is (= (.hashCode dyn) (.hashCode cm)) "hash")
        (is (= (.toString dyn) (.toString cm)) "TextFormat")
        (is (= (.getAllFields dyn) (.getAllFields cm)) "getAllFields")
        (is (= (.toByteString dyn) (.toByteString cm)))
        (is (.isInitialized cm))
        (is (= "" (.getInitializationErrorString cm)))))))

(deftest the-reflective-read-api-matches-dynamic-message
  (let [[_ proto ^Message dyn] (first values)
        d (desc proto)
        cm (parse-with (compiled proto) (.toByteArray dyn))
        empty-cm (compiled proto)
        empty-dyn (dynamic proto)]
    (doseq [^Descriptors$FieldDescriptor f (.getFields d)]
      (testing (.getName f)
        (if (.isRepeated f)
          (do (is (= (.getRepeatedFieldCount dyn f) (.getRepeatedFieldCount cm f)))
              (dotimes [i (.getRepeatedFieldCount dyn f)]
                (is (= (.getRepeatedField dyn f i) (.getRepeatedField cm f i)))))
          (do (is (= (.hasField dyn f) (.hasField cm f)))
              (is (= (.hasField empty-dyn f) (.hasField empty-cm f)))))
        (is (= (.getField dyn f) (.getField cm f)) "set value")
        (is (= (.getField empty-dyn f) (.getField empty-cm f)) "default value, including declared defaults and nested default instances")))
    (testing "oneofs"
      (let [oo (.getOneofs d)]
        (doseq [o oo]
          (is (= (.hasOneof dyn o) (.hasOneof cm o)))
          (is (= (.getOneofFieldDescriptor dyn o) (.getOneofFieldDescriptor cm o))))))
    (testing "hasField on a repeated field is refused, as DynamicMessage refuses it"
      (is (thrown? UnsupportedOperationException (.hasField cm (fd d "names")))))
    (testing "a field of another type is refused"
      (is (thrown? IllegalArgumentException (.getField cm (fd (desc wp3/Wire-prototype) "i32")))))))

(deftest the-reflective-write-api-builds-the-same-message
  (doseq [[name proto ^Message dyn] values]
    (testing name
      (let [b (.newBuilderForType (compiled proto))]
        (doseq [[f v] (.getAllFields dyn)]
          (.setField b f v))
        (.setUnknownFields b (.getUnknownFields dyn))
        (let [cm (.build b)]
          (is (.equals dyn cm))
          (is (java.util.Arrays/equals (.toByteArray dyn) (.toByteArray cm))))))))

(deftest text-format-parses-into-a-compiled-builder
  (doseq [[name proto ^Message dyn] values]
    (testing name
      (let [b (.newBuilderForType (compiled proto))]
        (TextFormat/merge (.toString dyn) b)
        (is (.equals dyn (.build b)))))))

(deftest builder-operations
  (let [proto (compiled wp3/Wire-prototype)
        d (desc proto)
        dyn-proto (dynamic wp3/Wire-prototype)]
    (testing "toBuilder round-trips and does not alias the message's collections"
      (let [m (parse-with proto (.toByteArray (wp3/Wire->proto {:names ["a"] :by-id {1 {:id "x"}}})))
            b (.toBuilder m)]
        (.addRepeatedField b (fd d "names") "b")
        (is (= ["a"] (.getField m (fd d "names"))) "the built message is untouched")
        (is (= ["a" "b"] (.getField (.build b) (fd d "names"))))
        (is (.equals m (.build (.toBuilder m))))))
    (testing "a builder that has built copies before mutating"
      (let [b (.newBuilderForType proto)]
        (.addRepeatedField b (fd d "names") "a")
        (let [m1 (.build b)]
          (.addRepeatedField b (fd d "names") "b")
          (is (= ["a"] (.getField m1 (fd d "names"))))
          (is (= ["a" "b"] (.getField (.build b) (fd d "names")))))))
    (testing "clear, clearField, clearOneof, setRepeatedField"
      (let [b (doto (.newBuilderForType proto)
                (.setField (fd d "i32") (int 5))
                (.setField (fd d "pick-str") "s")
                (.addRepeatedField (fd d "names") "a")
                (.addRepeatedField (fd d "names") "b"))]
        (.setRepeatedField b (fd d "names") 1 "B")
        (is (= ["a" "B"] (.getField b (fd d "names"))))
        (.clearOneof b (first (.getRealOneofs d)))
        (is (not (.hasOneof b (first (.getRealOneofs d)))))
        (.clearField b (fd d "names"))
        (is (= [] (.getField b (fd d "names"))))
        (is (.hasField b (fd d "i32")))
        (.clear b)
        (is (.equals proto (.build b)))))
    (testing "setting a oneof member clears the others, as DynamicMessage does"
      (let [set-both (fn [^Message$Builder b]
                       (-> b (.setField (fd d "pick-str") "s") (.setField (fd d "pick-i64") (long 7)) (.build)))
            cm (set-both (.newBuilderForType proto))
            dm (set-both (.newBuilderForType dyn-proto))]
        (is (.equals dm cm))
        (is (not (.hasField cm (fd d "pick-str"))))))
    (testing "a default on a field without presence is absence, as in DynamicMessage"
      (let [cm (-> (.newBuilderForType proto) (.setField (fd d "i32") (int 0)) (.build))
            dm (-> (.newBuilderForType dyn-proto) (.setField (fd d "i32") (int 0)) (.build))]
        (is (not (.hasField cm (fd d "i32"))))
        (is (.equals dm cm))
        (is (zero? (.getSerializedSize cm)))))
    (testing "enums are accepted as descriptors or numbers, read back as descriptors"
      (let [et (.getEnumType (fd d "color"))
            by-evd (-> (.newBuilderForType proto) (.setField (fd d "color") (.findValueByNumber et 2)) (.build))]
        (is (= (.findValueByNumber et 2) (.getField by-evd (fd d "color"))))
        (is (= 2 (.getNumber ^Descriptors$EnumValueDescriptor (.getField by-evd (fd d "color")))))))
    (testing "newBuilderForField gives a compiled builder for messages and an entry builder for maps"
      (let [b (.newBuilderForType proto)
            leaf-b (.newBuilderForField b (fd d "leaf"))
            entry-b (.newBuilderForField b (fd d "by-id"))]
        (is (not (instance? DynamicMessage (.build leaf-b))))
        (is (= (.getMessageType (fd d "by-id")) (.getDescriptorForType entry-b)))
        (is (thrown? UnsupportedOperationException (.newBuilderForField b (fd d "i32"))))))))

(deftest merge-from-follows-protobuf-semantics
  (let [proto (compiled wp3/Wire-prototype)
        d (desc proto)
        a (wp3/Wire->proto {:i32 1 :str "a" :names ["a"] :leaf {:id "a"} :by-id {1 {:id "one"}} :pick-str "s"})
        b (wp3/Wire->proto {:i32 2 :names ["b"] :leaf {:id "b"} :by-id {2 {:id "two"}} :pick-i64 9})
        expected (-> (.toBuilder a) (.mergeFrom b) (.build))]
    (testing "compiled into compiled"
      (let [ca (parse-with proto (.toByteArray a))
            cb (parse-with proto (.toByteArray b))
            merged (-> (.toBuilder ca) (.mergeFrom cb) (.build))]
        (is (.equals expected merged))
        (is (= ["a" "b"] (.getField merged (fd d "names"))) "repeateds append")
        (is (= 2 (.getField merged (fd d "i32"))) "scalars overwrite")
        (is (= "b" (.getField ^Message (.getField merged (fd d "leaf")) (fd (desc wp3/Leaf-prototype) "id"))) "messages merge")
        (is (= 2 (.getRepeatedFieldCount merged (fd d "by-id"))) "maps put")
        (is (not (.hasField merged (fd d "pick-str"))) "a oneof member from the other side wins")
        (is (= 9 (.getField merged (fd d "pick-i64"))))))
    (testing "a DynamicMessage into a compiled builder, through the reflective API"
      (let [ca (parse-with proto (.toByteArray a))
            merged (-> (.toBuilder ca) (.mergeFrom ^Message b) (.build))]
        (is (.equals expected merged))))
    (testing "bytes into a builder that has built: the message keeps its lists"
      (let [bld (.toBuilder (parse-with proto (.toByteArray a)))
            m1 (.build bld)]
        (.mergeFrom bld (.toByteArray b))
        (is (= ["a"] (.getField m1 (fd d "names"))))
        (is (.equals expected (.build bld)))))
    (testing "unknown fields merge"
      (let [u (-> (UnknownFieldSet/newBuilder) (.mergeVarintField 1000 7) (.build))
            m (-> (.newBuilderForType proto) (.mergeUnknownFields u) (.build))]
        (is (= [7] (.getVarintList (.getField (.getUnknownFields m) 1000))))))))

(deftest required-fields
  (let [proto (compiled wp2/Required-prototype)
        d (desc proto)
        dyn-proto (dynamic wp2/Required-prototype)]
    (testing "build refuses, with DynamicMessage's error string"
      (let [b (-> (.newBuilderForType proto) (.setField (fd d "n") (int 1)))
            db (-> (.newBuilderForType dyn-proto) (.setField (fd d "n") (int 1)))]
        (is (not (.isInitialized b)))
        (is (= (.getInitializationErrorString db) (.getInitializationErrorString b)))
        (is (= ["id"] (.findInitializationErrors b)))
        (is (thrown? UninitializedMessageException (.build b)))
        (is (some? (.buildPartial b)))))
    (testing "parseFrom refuses bytes missing a required field; parsePartialFrom accepts them"
      (let [partial-bytes (.toByteArray (.buildPartial (-> (.newBuilderForType proto) (.setField (fd d "n") (int 1)))))
            p ^Parser (.getParserForType proto)]
        (is (thrown? InvalidProtocolBufferException (.parseFrom p ^bytes partial-bytes)))
        (is (not (.isInitialized ^Message (.parsePartialFrom p ^bytes partial-bytes))))
        (testing "and core/decode reports it as a parse error"
          (is (= :parse (try (pb/decode proto partial-bytes) nil
                             (catch clojure.lang.ExceptionInfo e (:clj-protobuf/error (ex-data e)))))))))))

(deftest parser-overloads-agree
  (let [proto (compiled wp3/Wire-prototype)
        p ^Parser (.getParserForType proto)
        ^Message m (parse-with proto (.toByteArray (wp3/Wire->proto {:i32 7 :names ["a" "b"]})))
        bs (.toByteArray m)]
    (is (.equals m (.parseFrom p ^bytes bs)))
    (is (.equals m (.parseFrom p (ByteString/copyFrom bs))))
    (is (.equals m (.parseFrom p (ByteBuffer/wrap bs))))
    (is (.equals m (.parseFrom p (ByteArrayInputStream. bs))))
    (is (.equals m (.parseFrom p (CodedInputStream/newInstance bs))))
    (is (.equals m (.parseFrom p ^bytes bs (ExtensionRegistryLite/getEmptyRegistry))))
    (let [padded (byte-array (concat [9 9 9] (seq bs) [9]))]
      (is (.equals m (.parseFrom p padded 3 (alength bs)))))
    (testing "delimited: two messages on one stream, written by writeDelimitedTo"
      (let [out (ByteArrayOutputStream.)]
        (.writeDelimitedTo m out)
        (.writeDelimitedTo proto out)
        (let [in (ByteArrayInputStream. (.toByteArray out))]
          (is (.equals m (.parseDelimitedFrom p in)))
          (is (.equals proto (.parseDelimitedFrom p in)))
          (is (nil? (.parseDelimitedFrom p in)) "end of stream")
          (testing "and mergeDelimitedFrom on a builder"
            (let [in (ByteArrayInputStream. (.toByteArray out))
                  b (.newBuilderForType proto)]
              (is (true? (.mergeDelimitedFrom b in)))
              (is (.equals m (.build b))))))))
    (testing "trailing garbage is a parse error for parseFrom"
      (is (thrown? InvalidProtocolBufferException
                   (.parseFrom p ^bytes (byte-array (concat (seq bs) [(unchecked-byte 0xff)]))))))))

(deftest grpc-marshaller-call-pattern
  (testing "inbound: getParserForType().parseFrom(CodedInputStream, registry); outbound:
            getSerializedSize() then writeTo(OutputStream) into a stream that trusts
            the size — off by one byte and grpc fails"
    (doseq [[name proto ^Message dyn] values]
      (testing name
        (let [cm (parse-with (compiled proto) (.toByteArray dyn))
              size (.getSerializedSize cm)
              written (atom 0)
              sink (proxy [OutputStream] []
                     (write
                       ([b] (swap! written inc))
                       ([^bytes bs off len] (swap! written + len))))]
          (.writeTo cm ^OutputStream sink)
          (is (= size @written) "writeTo(OutputStream) wrote exactly getSerializedSize() bytes")
          (let [in (CodedInputStream/newInstance (.toByteArray cm))
                back (.parseFrom ^Parser (.getParserForType cm) in (ExtensionRegistryLite/getEmptyRegistry))]
            (is (.equals cm back))
            (is (.isAtEnd in))))))))

(deftest unknown-fields-are-carried-and-hashed
  (let [proto (compiled wp3/Wire-prototype)
        dyn-proto (dynamic wp3/Wire-prototype)
        bs (let [out (ByteArrayOutputStream.) cos (CodedOutputStream/newInstance out)]
             (.writeInt32 cos 1 5) (.writeInt32 cos 1000 7) (.flush cos) (.toByteArray out))
        cm (parse-with proto bs)
        dm (parse-with dyn-proto bs)]
    (is (= [7] (.getVarintList (.getField (.getUnknownFields cm) 1000))))
    (is (.equals dm cm))
    (is (= (.hashCode dm) (.hashCode cm)))
    (is (java.util.Arrays/equals bs (.toByteArray cm)))
    (is (= (.toString dm) (.toString cm)))))

(deftest default-instances
  (let [proto (compiled kitchen/Kitchen-prototype)]
    (is (.equals (dynamic kitchen/Kitchen-prototype) proto))
    (is (zero? (.getSerializedSize proto)))
    (is (.equals proto (.getDefaultInstanceForType (parse-with proto (.toByteArray (kitchen/Kitchen->proto {:int-field 1}))))))
    (is (= (UnknownFieldSet/getDefaultInstance) (.getUnknownFields proto)))
    (testing "an unset message field reads as the nested default instance, which is compiled too"
      (let [nested (.getField proto (fd (desc proto) "msg-field"))]
        (is (not (instance? DynamicMessage nested)))
        (is (.equals (dynamic kitchen/Nested-prototype) nested))))))

(deftest the-runtime-prototype-is-shared-per-descriptor
  (is (identical? (.getParserForType (compiled wp3/Wire-prototype))
                  (.getParserForType (compiled wp3/Wire-prototype))))
  (testing "rt/message hands out the compiled arm without a usable class hint"
    (let [proto (rt/message wp3/file-descriptor "Wire")]
      (is (message/compiled-message? proto))
      (is (identical? (.getParserForType proto) (.getParserForType (compiled wp3/Wire-prototype))))))
  (testing "and DynamicMessage on request, for reference"
    (is (instance? DynamicMessage (rt/dynamic-message wp3/file-descriptor "Wire")))))
