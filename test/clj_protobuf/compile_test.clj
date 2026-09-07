(ns clj-protobuf.compile-test
  "The codec compiler: the tables it builds from a descriptor, checked
  field by field on the wire fixtures, and the parse and write loops over
  them, checked against DynamicMessage on the same bytes. Nested types
  parse through DynamicMessage's own parsers here — the message layer that
  supplies the compiled parser is the next phase — so the slots hold
  DynamicMessages for message fields, and everything else in slot form.

  No generated Java classes: this runs on the plain-clj leg. It does not
  type-hint the compiler's records, for the reason wire_test gives."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.core :as pb]
            [clj-protobuf.impl.compile :as compile]
            [clj-protobuf.impl.wire :as wire]
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
            Descriptors$FieldDescriptor
            DynamicMessage
            Message
            UnknownFieldSet
            UnknownFieldSet$Builder
            WireFormat]
           [java.io ByteArrayOutputStream]
           [java.util ArrayList LinkedHashMap]))

;; ---------------------------------------------------------------------------
;; A compiler whose nested parsers are DynamicMessage's

(def compile-dynamic
  (compile/compiler
   (fn [ct]
     (delay (.getParserForType (DynamicMessage/getDefaultInstance (:descriptor @ct)))))))

(defn- desc ^Descriptors$Descriptor [^Message proto] (.getDescriptorForType proto))
(defn- fd
  "By proto name; kebab spellings are accepted for readability."
  ^Descriptors$FieldDescriptor [^Descriptors$Descriptor d ^String n]
  (or (.findFieldByName d (clojure.string/replace n "-" "_"))
      (throw (ex-info (str "no field " n) {}))))
(defn- field [t ^String n] (aget ^objects (:fields t) (.getIndex (fd (:descriptor t) n))))

(defn- parse
  "[slots unknown-builder-or-nil] from bytes, through the compiled type."
  [t ^bytes bs]
  (let [slots (object-array (compile/field-count t))
        in (CodedInputStream/newInstance bs)
        unknown (compile/read-into t in slots)]
    (.checkLastTagWas in 0)
    [slots unknown]))

(defn- write ^bytes [t ^objects slots ^UnknownFieldSet unknown]
  (let [out (ByteArrayOutputStream.)
        cos (CodedOutputStream/newInstance out)]
    (compile/write-slots t slots unknown cos)
    (.flush cos)
    (let [bs (.toByteArray out)]
      (is (= (alength bs) (compile/size-slots t slots unknown)) "size agrees with bytes")
      bs)))

(defn- round-trip
  "Parse bytes into slots and write them back."
  ^bytes [t ^bytes bs]
  (let [[slots unknown] (parse t bs)]
    (write t slots (some-> ^UnknownFieldSet$Builder unknown .build))))

(defn- slot-of [t slots ^String n] (aget ^objects slots (.getIndex (fd (:descriptor t) n))))

(defn- tags [^bytes bs]
  (let [in (CodedInputStream/newInstance bs)]
    (loop [acc []]
      (let [tag (.readTag in)]
        (if (zero? tag) acc (do (.skipField in tag) (recur (conj acc tag))))))))

;; ---------------------------------------------------------------------------
;; What the compiler decides

(deftest tables-follow-the-descriptor
  (let [p2 (compile-dynamic (desc wp2/Wire-prototype))
        p3 (compile-dynamic (desc wp3/Wire-prototype))
        e24 (compile-dynamic (desc we/Wire-prototype))
        k (compile-dynamic (desc kitchen/Kitchen-prototype))]
    (testing "one slot per field, in descriptor order"
      (is (= 36 (compile/field-count p2)))
      (is (= (.getIndex (fd (:descriptor p2) "huge")) (:slot (field p2 "huge")))))
    (testing "writers are in field-number order, whatever the declaration order"
      (is (= (sort (map :number (:writers p2))) (map :number (:writers p2))))
      (is (= 536870911 (:number (last (:writers p2))))))
    (testing "the tag table is dense when field numbers are small, sparse otherwise"
      (is (pos? (:dense k)))
      (is (pos? (:dense e24)))
      (is (zero? (:dense p2)) "the largest field number forces the sparse table")
      (is (some? (:tags p2)))
      (is (nil? (:tags k))))
    (testing "kinds, groups, presence, requiredness"
      (is (= :int (:kind (field p2 "i32"))))
      (is (= :long (:kind (field p2 "u64"))))
      (is (= :float (:kind (field p2 "flt"))))
      (is (= :bytes (:kind (field p2 "raw"))))
      (is (= :enum (:kind (field p2 "color"))))
      (is (= :message (:kind (field p2 "leaf"))))
      (is (:group? (field p2 "grp")))
      (is (:group? (field e24 "delimited")))
      (is (not (:group? (field e24 "length-prefixed"))))
      (is (:has-presence? (field p2 "i32")))
      (is (not (:has-presence? (field p3 "i32"))))
      (is (:has-presence? (field p3 "opt-i32")))
      (is (not (:has-presence? (field e24 "implicit"))))
      (is (:has-presence? (field e24 "explicit")))
      (is (= [0] (vec (:required-slots (compile-dynamic (desc wp2/Required-prototype))))))
      (is (empty? (:required-slots p2))))
    (testing "defaults in slot representation: enums are numbers"
      (is (= 42 (:default (field p2 "dflt-int"))))
      (is (= "dflt" (:default (field p2 "dflt-str"))))
      (is (= 2 (:default (field p2 "dflt-enum"))))
      (is (= 0 (:default (field p3 "i32"))))
      (is (= "" (:default (field p3 "str"))))
      (is (= ByteString/EMPTY (:default (field p3 "raw"))))
      (is (nil? (:default (field p3 "leaf"))))
      (is (nil? (:default (field p3 "names")))))
    (testing "repeated and map flags, map types"
      (is (:repeated? (field p2 "names")))
      (is (not (:repeated? (field p2 "by-id"))))
      (is (:map? (field p2 "by-id")))
      (is (= "INT32" (:key-type (field p2 "by-id"))))
      (is (= "MESSAGE" (:val-type (field p2 "by-id"))))
      (is (= "ENUM" (:val-type (field p2 "by-color"))))
      (is (some? (:val-enum-type (field p2 "by-color")))))
    (testing "oneofs: members know their oneof, the type knows the members"
      (is (= 0 (:oneof (field p3 "pick-str"))))
      (is (= 0 (:oneof (field p3 "pick-leaf"))))
      (is (= -1 (:oneof (field p3 "i32"))))
      (is (= -1 (:oneof (field p3 "opt-i32"))) "proto3 optional's synthetic oneof is not a oneof")
      (is (= 1 (alength ^objects (:oneof-slots p3))))
      (is (= (set (map #(.getIndex (fd (:descriptor p3) %)) ["pick_str" "pick_i64" "pick_leaf"]))
             (set (aget ^objects (:oneof-slots p3) 0)))))
    (testing "closed enums mean the parse loop must carry an unknown-field builder"
      (is (:needs-unknown? p2))
      (is (:needs-unknown? e24))
      (is (not (:needs-unknown? p3))))))

(deftest a-cyclic-descriptor-compiles
  (let [struct-fd (rt/known-file "google/protobuf/struct.proto")
        struct-d (.findMessageTypeByName struct-fd "Struct")
        t (compile-dynamic struct-d)]
    (is (some? t) "Struct -> Value -> Struct terminates because nesting is lazy")
    (is (identical? t (compile-dynamic struct-d)) "and is cached by identity")))

;; ---------------------------------------------------------------------------
;; The loops, against DynamicMessage bytes

(def p2-value
  {:i32 -1 :i64 -2 :u32 3 :u64 4 :s32 -5 :s64 -6 :f32 7 :f64 8 :sf32 -9 :sf64 -10
   :flt (float 1.5) :dbl 2.5 :flag true :str "héllo 日本 😀" :raw (byte-array [0 127 -128])
   :color :CLOSED_B :leaf {:id "leaf"}
   :unpacked [1 -2 300] :packed [4 5] :packed-s64 [-1 Long/MAX_VALUE] :packed-dbl [0.5 -0.0]
   :colors [:CLOSED_A :CLOSED_B] :leaves [{:id "a"} {:id "b"}] :names ["x" "" "日本"]
   :by-id {2 {:id "two"} -1 {:id "minus"}} :by-color {"a" :CLOSED_A "b" :CLOSED_UNSPECIFIED}
   :pick-leaf {:id "picked"}
   :dflt-int 42 :dflt-str "other" :dflt-enum :CLOSED_A
   :grp {:note "g"} :grps [{:n 1} {:n 2}]
   :high "high" :huge 7})

(def p3-value
  {:i32 -1 :i64 -2 :u32 3 :u64 4 :s32 -5 :s64 -6 :f32 7 :f64 8 :sf32 -9 :sf64 -10
   :flt (float 1.5) :dbl 2.5 :flag true :str "s" :raw (byte-array [1 2])
   :color :OPEN_B :leaf {:id "leaf"}
   :packed [1 -2 300] :unpacked [4 5] :packed-s64 [-1] :packed-dbl [0.5]
   :colors [:OPEN_A :OPEN_B] :leaves [{:id "a"}] :names ["x"]
   :by-id {1 {:id "one"}} :by-color {"k" :OPEN_A}
   :pick-i64 99 :opt-i32 0 :opt-str ""
   :high "h" :huge 1})

(def e24-value
  {:str "s" :checked "c" :expanded [1 2] :packed [3 4]
   :delimited {:id "d"} :length-prefixed {:id "l"}
   :implicit 5 :explicit 0 :open :OPEN_A :closed :CLOSED_B :closed-list [:CLOSED_A]})

(def kitchen-value
  {:str-field "hello" :int-field 42 :bool-field true :bytes-field (byte-array [1 2 3])
   :dbl-field 3.5 :long-field 9007199254740993 :enum-field :COLOR_RED
   :msg-field {:id "nested"} :tags ["a" "b"] :children [{:id "c1"} {:id "c2"}]
   :counts {"x" 1 "y" 2} :choice-int 7 :ts {:seconds 5 :nanos 100} :dur {:seconds 30}
   :wrapped {:value "wrapped"} :implicit-field "imp" :delimited {:note "grouped"}})

(deftest round-trips-are-byte-identical-to-dynamic-message
  (doseq [[name proto ->proto value]
          [["wire.p2/Wire" wp2/Wire-prototype wp2/Wire->proto p2-value]
           ["wire.p3/Wire" wp3/Wire-prototype wp3/Wire->proto p3-value]
           ["wire.e2024/Wire" we/Wire-prototype we/Wire->proto e24-value]
           ["e2024/Kitchen" kitchen/Kitchen-prototype kitchen/Kitchen->proto kitchen-value]
           ["nested/Outer" nested/Outer-prototype nested/Outer->proto
            {:id "o" :counts {"k" 1} :inner {:name "i" :labels {"env" "prod"} :innermost {:depth 3}}}]]]
    (testing name
      (let [t (compile-dynamic (desc proto))
            bs (pb/encode (->proto value))]
        (is (java.util.Arrays/equals bs (round-trip t bs)))))))

(deftest slots-hold-the-documented-representations
  (let [t (compile-dynamic (desc wp2/Wire-prototype))
        [slots unknown] (parse t (pb/encode (wp2/Wire->proto p2-value)))]
    (is (empty? (.asMap (.build ^UnknownFieldSet$Builder unknown)))
        "nothing was unknown (the builder exists up front: this type has a closed enum)")
    (is (instance? Integer (slot-of t slots "i32")))
    (is (instance? Long (slot-of t slots "u64")))
    (is (instance? Float (slot-of t slots "flt")))
    (is (instance? Boolean (slot-of t slots "flag")))
    (is (instance? String (slot-of t slots "str")))
    (is (instance? ByteString (slot-of t slots "raw")))
    (is (= 2 (slot-of t slots "color")) "enums are numbers")
    (is (instance? DynamicMessage (slot-of t slots "leaf")) "nested through the supplied parser")
    (is (instance? ArrayList (slot-of t slots "unpacked")))
    (is (= [1 -2 300] (slot-of t slots "unpacked")))
    (is (= [1 2] (slot-of t slots "colors")) "repeated enums are numbers too")
    (is (instance? LinkedHashMap (slot-of t slots "by-id")))
    (is (= [2 -1] (vec (.keySet ^LinkedHashMap (slot-of t slots "by-id")))) "insertion order is wire order")
    (is (= {"a" 1 "b" 0} (slot-of t slots "by-color")))
    (is (nil? (slot-of t slots "pick-str")))
    (is (instance? DynamicMessage (slot-of t slots "pick-leaf")))
    (is (instance? DynamicMessage (slot-of t slots "grp")))
    (is (= 2 (count (slot-of t slots "grps"))))
    (is (= 7 (slot-of t slots "huge")))))

(deftest fields-without-presence-normalize-their-default-to-nil
  (let [t (compile-dynamic (desc wp3/Wire-prototype))
        explicit-zero (let [out (ByteArrayOutputStream.) cos (CodedOutputStream/newInstance out)]
                        (.writeInt32 cos 1 0) (.writeString cos 14 "") (.writeInt32 cos 30 0)
                        (.flush cos) (.toByteArray out))
        [slots unknown] (parse t explicit-zero)]
    (is (nil? (slot-of t slots "i32")) "an explicit default on the wire is absence")
    (is (nil? (slot-of t slots "str")))
    (is (= 0 (slot-of t slots "opt-i32")) "but explicit presence keeps it")
    (let [re (write t slots nil)]
      (is (= [(wire/tag 30 WireFormat/WIRETYPE_VARINT)] (tags re)) "and only the present field is re-encoded")
      (is (java.util.Arrays/equals re (.toByteArray (DynamicMessage/parseFrom (desc wp3/Wire-prototype) explicit-zero)))
          "which is exactly DynamicMessage's re-encoding"))))

(deftest both-encodings-of-a-repeated-scalar-parse
  (let [p2 (compile-dynamic (desc wp2/Wire-prototype))
        p3-bytes (pb/encode (wp3/Wire->proto {:packed [1 -2 300] :unpacked [4 5]}))
        [slots _] (parse p2 p3-bytes)]
    (is (= [1 -2 300] (slot-of p2 slots "unpacked")) "proto3's packed run, read by proto2's expanded field")
    (is (= [4 5] (slot-of p2 slots "packed")) "and the reverse")
    (testing "re-encoded the way this descriptor writes"
      (is (java.util.Arrays/equals (write p2 slots nil)
                                   (pb/encode (wp2/Wire->proto {:unpacked [1 -2 300] :packed [4 5]})))))))

(deftest a-oneof-member-read-off-the-wire-clears-its-siblings
  (let [t (compile-dynamic (desc wp3/Wire-prototype))
        both (byte-array (concat (seq (pb/encode (wp3/Wire->proto {:pick-str "first"})))
                                 (seq (pb/encode (wp3/Wire->proto {:pick-i64 2})))))
        [slots _] (parse t both)]
    (is (nil? (slot-of t slots "pick-str")))
    (is (= 2 (slot-of t slots "pick-i64")))
    (is (java.util.Arrays/equals (write t slots nil)
                                 (.toByteArray (DynamicMessage/parseFrom (desc wp3/Wire-prototype) both))))))

(deftest unknown-fields-round-trip
  (let [t (compile-dynamic (desc wp3/Wire-prototype))
        known (pb/encode (wp3/Wire->proto {:i32 5 :names ["a"]}))
        bs (let [out (ByteArrayOutputStream.) cos (CodedOutputStream/newInstance out)]
             (.writeRawBytes cos known)
             (.writeInt32 cos 1000 7)
             (.writeFixed32 cos 1001 (int 0xCAFE))
             (.writeBytes cos 1002 (ByteString/copyFromUtf8 "unknown"))
             (.writeFixed64 cos 1003 (long 0xDEADBEEF))
             (.writeTag cos 1004 WireFormat/WIRETYPE_START_GROUP)
             (.writeInt32 cos 1 1)
             (.writeTag cos 1004 WireFormat/WIRETYPE_END_GROUP)
             (.flush cos) (.toByteArray out))
        [slots unknown] (parse t bs)]
    (is (some? unknown))
    (is (= #{1000 1001 1002 1003 1004} (set (keys (.asMap (.build ^UnknownFieldSet$Builder unknown))))))
    (is (= 5 (slot-of t slots "i32")))
    (is (java.util.Arrays/equals (write t slots (.build ^UnknownFieldSet$Builder unknown))
                                 (.toByteArray (DynamicMessage/parseFrom (desc wp3/Wire-prototype) bs)))
        "known fields, then unknown fields — DynamicMessage's order too")))

(deftest closed-enum-unknown-numbers-become-unknown-fields
  (let [t (compile-dynamic (desc wp2/Wire-prototype))
        bs (pb/encode (wp3/Wire->proto {:color 7 :colors [1 7 2]}))
        [slots unknown] (parse t bs)]
    (is (nil? (slot-of t slots "color")))
    (is (= [1 2] (slot-of t slots "colors")))
    (is (= #{16 22} (set (keys (.asMap (.build ^UnknownFieldSet$Builder unknown))))))
    (is (java.util.Arrays/equals (write t slots (.build ^UnknownFieldSet$Builder unknown))
                                 (.toByteArray (DynamicMessage/parseFrom (desc wp2/Wire-prototype) bs))))))

(deftest the-parse-loop-stops-at-an-end-group-tag
  (let [wire-t (compile-dynamic (desc wp2/Wire-prototype))
        grp-t (compile-dynamic (.getMessageType (fd (desc wp2/Wire-prototype) "grp")))
        bs (pb/encode (wp2/Wire->proto {:grp {:note "g"} :high "after"}))
        in (CodedInputStream/newInstance bs)
        start (.readTag in)
        slots (object-array (compile/field-count grp-t))]
    (is (= (wire/tag 33 WireFormat/WIRETYPE_START_GROUP) start))
    (compile/read-into grp-t in slots)
    (is (= "g" (aget slots 0)))
    (.checkLastTagWas in (unchecked-int (wire/tag 33 WireFormat/WIRETYPE_END_GROUP)))
    (is (= (wire/tag 2047 WireFormat/WIRETYPE_LENGTH_DELIMITED) (.readTag in))
        "the input is positioned right after the group")
    (is (some? wire-t))))

(deftest empty-collections-do-not-reach-the-wire
  (let [t (compile-dynamic (desc wp2/Wire-prototype))
        slots (object-array (compile/field-count t))]
    (aset slots (.getIndex (fd (:descriptor t) "packed")) (ArrayList.))
    (aset slots (.getIndex (fd (:descriptor t) "by_id")) (LinkedHashMap.))
    (is (zero? (alength (write t slots nil))))))
