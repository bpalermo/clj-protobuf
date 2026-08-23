(ns clj-protobuf.byte-identity-test
  "The serialized bytes must be protoc's bytes.

  The reference is protobuf-java's own generated code over the same .proto
  files (//test/proto:fixtures_java_proto). Having that jar on the classpath is
  also what turns the Java-class-hint fast path ON — the goldens' rt/message
  calls name exactly these classes — so this suite exercises the hinted arm,
  and rebuilds the same values through 2-arity (DynamicMessage) prototypes to
  prove both arms produce identical bytes. Same codec, same field descriptors,
  same bytes."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.codec :as codec]
            [clj-protobuf.core :as pb]
            [clj-protobuf.runtime :as rt]
            [fixtures.e2024.kitchen :as e2024])
  (:import [com.acme.fixtures.e2024 Color Delimited Kitchen Nested]
           [com.google.protobuf ByteString Duration DynamicMessage Message
            StringValue Timestamp]))

(def kitchen-value
  {:str-field "hello"
   :int-field 42
   :bool-field true
   :bytes-field (byte-array [1 2 3])
   :dbl-field 3.5
   :long-field 9007199254740993
   :enum-field :COLOR_RED
   :msg-field {:id "nested"}
   :tags ["a" "b"]
   :children [{:id "c1"} {:id "c2"}]
   :counts {"x" 1 "y" 2}
   :choice-int 7
   :ts {:seconds 5 :nanos 100}
   :dur {:seconds 30}
   :wrapped {:value "wrapped"}
   :implicit-field "imp"
   :delimited {:note "grouped"}})

(defn- java-reference
  "The same value through protoc's generated Java builders — the authority."
  ^Message []
  (-> (Kitchen/newBuilder)
      (.setStrField "hello")
      (.setIntField 42)
      (.setBoolField true)
      (.setBytesField (ByteString/copyFrom (byte-array [1 2 3])))
      (.setDblField 3.5)
      (.setLongField 9007199254740993)
      (.setEnumField Color/COLOR_RED)
      (.setMsgField (-> (Nested/newBuilder) (.setId "nested") (.build)))
      (.addTags "a")
      (.addTags "b")
      (.addChildren (-> (Nested/newBuilder) (.setId "c1") (.build)))
      (.addChildren (-> (Nested/newBuilder) (.setId "c2") (.build)))
      (.putCounts "x" 1)
      (.putCounts "y" 2)
      (.setChoiceInt 7)
      (.setTs (-> (Timestamp/newBuilder) (.setSeconds 5) (.setNanos 100) (.build)))
      (.setDur (-> (Duration/newBuilder) (.setSeconds 30) (.build)))
      (.setWrapped (-> (StringValue/newBuilder) (.setValue "wrapped") (.build)))
      (.setImplicitField "imp")
      (.setDelimited (-> (Delimited/newBuilder) (.setNote "grouped") (.build)))
      (.build)))

(deftest hint-path-is-engaged
  (testing "with the generated classes on the classpath, the prototype IS the
            generated class, not DynamicMessage"
    (is (instance? Kitchen e2024/Kitchen-prototype))
    (is (not (instance? DynamicMessage e2024/Kitchen-prototype)))))

(deftest hinted-arm-matches-protoc
  (is (java.util.Arrays/equals
       (pb/encode (java-reference))
       (pb/encode (e2024/Kitchen->proto kitchen-value)))))

(defn- dynamic-kitchen->proto
  "The same conversion the generated ->proto does, but against a 2-arity
  (hint-free) prototype: the pure DynamicMessage arm, independent of what is
  on the classpath."
  ^Message [m]
  (let [proto (rt/message e2024/file-descriptor "Kitchen")
        b     (.newBuilderForType ^Message proto)]
    (doseq [fname ["str_field" "int_field" "bool_field" "bytes_field"
                   "dbl_field" "long_field" "enum_field" "msg_field" "tags"
                   "children" "counts" "choice_str" "choice_int" "choice_msg"
                   "ts" "dur" "wrapped" "implicit_field" "delimited"]]
      (let [h (rt/field proto fname)]
        (codec/set-field! b h (get m (:kebab-key h)) nil)))
    (.build b)))

(deftest dynamic-arm-matches-protoc
  (is (java.util.Arrays/equals
       (pb/encode (java-reference))
       (pb/encode (dynamic-kitchen->proto kitchen-value)))))

(deftest arms-agree-after-parsing
  (testing "bytes parse and re-encode identically through either arm. The two
            arms live in different descriptor pools (the generated classes' and
            the embedded descriptor's), so each converts with its own handles —
            mixing pools is invalid, by protobuf-java's own rules — but the
            wire bytes are the meeting point and must agree."
    (let [bytes (pb/encode (e2024/Kitchen->proto kitchen-value))
          via-hinted  (pb/decode e2024/Kitchen-prototype bytes)
          via-dynamic (pb/decode (rt/message e2024/file-descriptor "Kitchen") bytes)]
      (is (java.util.Arrays/equals (pb/encode via-hinted)
                                   (pb/encode via-dynamic))))))

(deftest delimited-wire-format
  (testing "editions DELIMITED really is group encoding on the wire — protoc's
            bytes for the field prove it, ours must match exactly"
    (let [ours   (pb/encode (e2024/Kitchen->proto {:delimited {:note "d"}}))
          theirs (pb/encode (-> (Kitchen/newBuilder)
                                (.setDelimited (-> (Delimited/newBuilder)
                                                   (.setNote "d")
                                                   (.build)))
                                (.build)))]
      (is (java.util.Arrays/equals theirs ours)))))
