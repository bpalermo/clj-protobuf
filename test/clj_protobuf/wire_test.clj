(ns clj-protobuf.wire-test
  "The wire layer, one primitive at a time, against protobuf-java on the same
  bytes: what our writer writes, DynamicMessage parses to the value; what
  DynamicMessage writes, our reader reads back; every writer's size is the
  length of what it wrote. The descriptors come from the wire fixtures, so
  each type name is exercised at a real field number, including the
  multi-byte ones."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.impl.wire :as wire]
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
            InvalidProtocolBufferException
            Message
            UnknownFieldSet
            UnknownFieldSet$Builder
            WireFormat]
           [java.io ByteArrayOutputStream]
           [java.util ArrayList LinkedHashMap]))

;; ---------------------------------------------------------------------------
;; Plumbing

(def ^Descriptors$Descriptor p2-wire (.getDescriptorForType ^Message wp2/Wire-prototype))
(def ^Descriptors$Descriptor p3-wire (.getDescriptorForType ^Message wp3/Wire-prototype))
(def ^Descriptors$Descriptor e24-wire (.getDescriptorForType ^Message we/Wire-prototype))

(defn- fd ^Descriptors$FieldDescriptor [^Descriptors$Descriptor d ^String n] (.findFieldByName d n))
(defn- type-name [^Descriptors$FieldDescriptor f] (.name (.getType f)))

(defn- bytes-of
  "What a writer writes, after checking its size claim against the bytes."
  ^bytes [w v]
  (let [out (ByteArrayOutputStream.)
        cos (CodedOutputStream/newInstance out)]
    (wire/write! w cos v)
    (.flush cos)
    (let [bs (.toByteArray out)]
      (is (= (alength bs) (wire/size-of w v)) "size agrees with the bytes written")
      bs)))

(defn- dyn-parse ^DynamicMessage [^Descriptors$Descriptor d ^bytes bs]
  (DynamicMessage/parseFrom d bs))

(defn- dyn-message ^DynamicMessage [^Descriptors$Descriptor d & kvs]
  (let [b (DynamicMessage/newBuilder d)]
    (doseq [[n v] (partition 2 kvs)]
      (.setField b (fd d n) v))
    (.build b)))

(defn- dyn-bytes ^bytes [^Descriptors$Descriptor d & kvs]
  (.toByteArray ^DynamicMessage (apply dyn-message d kvs)))

(defn- read-all
  "Drive a reader over every occurrence of its field in bs, in order, the
  way the compiled parser will; returns [slot-value unknown-field-set]."
  [r expected-tags ^bytes bs]
  (let [in (CodedInputStream/newInstance bs)
        unknown (UnknownFieldSet/newBuilder)
        tags (set expected-tags)]
    (loop [current nil]
      (let [t (.readTag in)]
        (cond
          (zero? t) [current (.build unknown)]
          (tags t) (recur (wire/read! r in current unknown))
          :else (do (.mergeFieldFrom unknown t in) (recur current)))))))

(defn- read-one [r tag ^bytes bs]
  (first (read-all r [tag] bs)))

(defn- parser-of [^Descriptors$Descriptor d]
  (delay (.getParserForType (DynamicMessage/getDefaultInstance d))))

;; ---------------------------------------------------------------------------
;; Scalars

(def ^:private scalar-values
  ;; field name, slot value, and what DynamicMessage.getField returns for it
  [["i32" (int -1)] ["i64" (long -1)] ["u32" (int -1)] ["u64" (long -1)]
   ["s32" (int -1)] ["s64" (long -1)] ["f32" (int -1)] ["f64" (long -1)]
   ["sf32" (int -1)] ["sf64" (long -1)]
   ["flt" (float 1.5)] ["dbl" 2.5] ["flag" true] ["str" "héllo 日本 😀"]
   ["raw" (ByteString/copyFrom (byte-array [0 127 -128]))]
   ["high" "past one byte of tag"] ["huge" (int 7)]])

(deftest scalar-writers-write-what-dynamic-message-parses
  (doseq [[n v] scalar-values
          :let [f (fd p2-wire n)]]
    (testing (str n " (" (type-name f) ")")
      (let [w (wire/scalar-writer (type-name f) (.getNumber f) :singular)
            parsed (dyn-parse p2-wire (bytes-of w v))]
        (is (.hasField parsed f))
        (is (= v (.getField parsed f)))))))

(deftest scalar-readers-read-what-dynamic-message-writes
  (doseq [[n v] scalar-values
          :let [f (fd p2-wire n)]]
    (testing (str n " (" (type-name f) ")")
      (let [r (wire/scalar-reader (type-name f) :singular {:utf8? true})
            t (wire/tag (.getNumber f) (wire/wire-type (type-name f)))]
        (is (= v (read-one r t (dyn-bytes p2-wire n v))))))))

(deftest enums-are-numbers-in-slots
  (let [f (fd p2-wire "color")
        w (wire/scalar-writer "ENUM" 16 :singular)
        parsed (dyn-parse p2-wire (bytes-of w (int 2)))]
    (is (= 2 (.getNumber ^Descriptors$EnumValueDescriptor (.getField parsed f))))
    (let [r (wire/scalar-reader "ENUM" :singular {})
          evd (.findValueByNumber (.getEnumType f) 2)]
      (is (= 2 (read-one r (wire/tag 16 WireFormat/WIRETYPE_VARINT) (dyn-bytes p2-wire "color" evd)))))))

(deftest tags-and-wire-types
  (is (= WireFormat/WIRETYPE_VARINT (wire/wire-type "SINT64")))
  (is (= WireFormat/WIRETYPE_FIXED32 (wire/wire-type "FLOAT")))
  (is (= WireFormat/WIRETYPE_START_GROUP (wire/wire-type "GROUP")))
  (is (= 16378 (wire/tag 2047 WireFormat/WIRETYPE_LENGTH_DELIMITED)))
  (is (= 2047 (WireFormat/getTagFieldNumber (unchecked-int (wire/tag 2047 2)))))
  (is (= 536870911 (WireFormat/getTagFieldNumber (unchecked-int (wire/tag 536870911 0))))))

;; ---------------------------------------------------------------------------
;; Repeated scalars: packed and expanded, either way round

(deftest packed-and-expanded-repeateds
  (let [vals (ArrayList. [(int 1) (int -2) (int 300)])
        packed-f (fd p3-wire "packed")      ; 18, packed in proto3
        unpacked-f (fd p2-wire "unpacked")] ; 18, expanded in proto2
    (testing "the packed writer's bytes are one length-delimited run"
      (let [bs (bytes-of (wire/scalar-writer "INT32" 18 :packed) vals)]
        (is (= [(wire/tag 18 WireFormat/WIRETYPE_LENGTH_DELIMITED)]
               (let [in (CodedInputStream/newInstance bs) t (.readTag in)] (.skipField in t) [t])))
        (is (= [1 -2 300] (.getField (dyn-parse p3-wire bs) packed-f)))
        (testing "and a proto2 descriptor accepts them for its expanded field"
          (is (= [1 -2 300] (.getField (dyn-parse p2-wire bs) unpacked-f))))))
    (testing "the expanded writer's bytes are one tag per element"
      (let [bs (bytes-of (wire/scalar-writer "INT32" 18 :expanded) vals)]
        (is (= [1 -2 300] (.getField (dyn-parse p2-wire bs) unpacked-f)))
        (is (= [1 -2 300] (.getField (dyn-parse p3-wire bs) packed-f)))))
    (testing "readers: packed reads DynamicMessage's packed run, expanded reads its tags, and each accepts the other's bytes"
      (let [p3-bytes (dyn-bytes p3-wire "packed" vals)
            p2-bytes (dyn-bytes p2-wire "unpacked" vals)
            packed-tag (wire/tag 18 WireFormat/WIRETYPE_LENGTH_DELIMITED)
            expanded-tag (wire/tag 18 WireFormat/WIRETYPE_VARINT)]
        (is (= [1 -2 300] (read-one (wire/scalar-reader "INT32" :packed {}) packed-tag p3-bytes)))
        (is (= [1 -2 300] (read-one (wire/scalar-reader "INT32" :expanded {}) expanded-tag p2-bytes)))
        (testing "a packed run appended to expanded elements accumulates into one list"
          (let [both (byte-array (concat (seq p2-bytes) (seq p3-bytes)))
                in (CodedInputStream/newInstance both)
                unknown (UnknownFieldSet/newBuilder)
                exp (wire/scalar-reader "INT32" :expanded {})
                pk (wire/scalar-reader "INT32" :packed {})]
            (is (= [1 -2 300 1 -2 300]
                   (loop [cur nil]
                     (let [t (.readTag in)]
                       (cond (zero? t) (vec cur)
                             (= t expanded-tag) (recur (wire/read! exp in cur unknown))
                             (= t packed-tag) (recur (wire/read! pk in cur unknown)))))))))))
    (testing "other packable types"
      (doseq [[n tn vs] [["packed_s64" "SINT64" (ArrayList. [(long -1) (long 1) Long/MIN_VALUE])]
                         ["packed_dbl" "DOUBLE" (ArrayList. [0.5 -0.0 1e300])]]
              :let [f (fd p2-wire n) num (.getNumber f)]]
        (testing n
          (is (= (vec vs) (.getField (dyn-parse p2-wire (bytes-of (wire/scalar-writer tn num :packed) vs)) f)))
          (is (= (vec vs) (.getField (dyn-parse p2-wire (bytes-of (wire/scalar-writer tn num :expanded) vs)) f)))
          (is (= (vec vs) (read-one (wire/scalar-reader tn :packed {})
                                    (wire/tag num WireFormat/WIRETYPE_LENGTH_DELIMITED)
                                    (dyn-bytes p2-wire n vs)))))))))

(deftest repeated-strings-are-expanded-only
  (let [f (fd p2-wire "names")
        vs (ArrayList. ["a" "" "日本"])
        bs (bytes-of (wire/scalar-writer "STRING" 24 :expanded) vs)]
    (is (= ["a" "" "日本"] (.getField (dyn-parse p2-wire bs) f)))
    (is (= ["a" "" "日本"]
           (read-one (wire/scalar-reader "STRING" :expanded {:utf8? true})
                     (wire/tag 24 WireFormat/WIRETYPE_LENGTH_DELIMITED)
                     (dyn-bytes p2-wire "names" vs))))))

;; ---------------------------------------------------------------------------
;; Strings: UTF-8 validation is the reader's choice, made once

(defn- string-field-bytes ^bytes [n ^bytes raw]
  (let [out (ByteArrayOutputStream.) cos (CodedOutputStream/newInstance out)]
    (.writeBytes cos n (ByteString/copyFrom raw)) (.flush cos) (.toByteArray out)))

(deftest utf8-validation-is-decided-per-reader
  (let [bad (byte-array [(unchecked-byte 0xff) (unchecked-byte 0xfe) 65])
        bs (string-field-bytes 14 bad)
        t (wire/tag 14 WireFormat/WIRETYPE_LENGTH_DELIMITED)]
    (is (thrown? InvalidProtocolBufferException
                 (read-one (wire/scalar-reader "STRING" :singular {:utf8? true}) t bs)))
    (is (= "��A" (read-one (wire/scalar-reader "STRING" :singular {:utf8? false}) t bs)))))

;; ---------------------------------------------------------------------------
;; Closed enums route undeclared numbers to unknown fields

(deftest closed-enum-readers-keep-unknown-numbers-unknown
  (let [known? #{0 1 2}
        opts {:known? known? :field-number 16}
        t (wire/tag 16 WireFormat/WIRETYPE_VARINT)]
    (testing "singular: the slot is untouched and the number is an unknown field"
      (let [bs (bytes-of (wire/scalar-writer "ENUM" 16 :singular) (int 7))
            [v unknown] (read-all (wire/scalar-reader "ENUM" :singular opts) [t] bs)]
        (is (nil? v))
        (is (= [7] (.getVarintList (.getField ^UnknownFieldSet unknown 16))))))
    (testing "a declared number is a value"
      (is (= 2 (read-one (wire/scalar-reader "ENUM" :singular opts) t
                         (bytes-of (wire/scalar-writer "ENUM" 16 :singular) (int 2))))))
    (testing "expanded and packed: declared numbers stay, the rest leave"
      (let [vals (ArrayList. [(int 1) (int 7) (int 2)])
            exp-bytes (bytes-of (wire/scalar-writer "ENUM" 22 :expanded) vals)
            pk-bytes (bytes-of (wire/scalar-writer "ENUM" 22 :packed) vals)
            opts22 {:known? known? :field-number 22}
            [ev eu] (read-all (wire/scalar-reader "ENUM" :expanded opts22) [(wire/tag 22 0)] exp-bytes)
            [pv pu] (read-all (wire/scalar-reader "ENUM" :packed opts22) [(wire/tag 22 2)] pk-bytes)]
        (is (= [1 2] ev))
        (is (= [7] (.getVarintList (.getField ^UnknownFieldSet eu 22))))
        (is (= [1 2] pv))
        (is (= [7] (.getVarintList (.getField ^UnknownFieldSet pu 22))))))
    (testing "and this is exactly what DynamicMessage does with the same bytes"
      (let [parsed (dyn-parse p2-wire (bytes-of (wire/scalar-writer "ENUM" 16 :singular) (int 7)))]
        (is (not (.hasField parsed (fd p2-wire "color"))))
        (is (= [7] (.getVarintList (.getField (.getUnknownFields parsed) 16))))))))

;; ---------------------------------------------------------------------------
;; Messages and groups

(def ^Descriptors$Descriptor leaf-d (.getDescriptorForType ^Message wp2/Leaf-prototype))
(def ^Descriptors$Descriptor grp-d (.getMessageType (fd p2-wire "grp")))
(def ^Descriptors$Descriptor grps-d (.getMessageType (fd p2-wire "grps")))

(deftest message-fields
  (let [leaf (dyn-message leaf-d "id" "x")
        f (fd p2-wire "leaf")]
    (testing "singular: length-delimited, parsed by DynamicMessage"
      (let [bs (bytes-of (wire/message-writer 17 :singular false) leaf)]
        (is (= "x" (.getField ^Message (.getField (dyn-parse p2-wire bs) f) (fd leaf-d "id"))))
        (is (= leaf (read-one (wire/message-reader* 17 :singular false (parser-of leaf-d))
                              (wire/tag 17 WireFormat/WIRETYPE_LENGTH_DELIMITED) bs)))))
    (testing "a singular message field repeated on the wire merges"
      (let [a (dyn-bytes p2-wire "leaf" (dyn-message leaf-d "id" "first"))
            b (dyn-bytes p2-wire "leaf" (dyn-message leaf-d "id" "second"))
            both (byte-array (concat (seq a) (seq b)))
            r (wire/message-reader* 17 :singular false (parser-of leaf-d))
            merged (read-one r (wire/tag 17 WireFormat/WIRETYPE_LENGTH_DELIMITED) both)]
        (is (= "second" (.getField ^Message merged (fd leaf-d "id"))))
        (is (= "second" (.getField ^Message (.getField (dyn-parse p2-wire both) f) (fd leaf-d "id"))))))
    (testing "repeated messages"
      (let [leaves (ArrayList. [(dyn-message leaf-d "id" "a") (dyn-message leaf-d "id" "b")])
            f (fd p2-wire "leaves")
            bs (bytes-of (wire/message-writer 23 :expanded false) leaves)]
        (is (= (vec leaves) (.getField (dyn-parse p2-wire bs) f)))
        (is (= (vec leaves) (read-one (wire/message-reader* 23 :expanded false (parser-of leaf-d))
                                      (wire/tag 23 WireFormat/WIRETYPE_LENGTH_DELIMITED)
                                      (dyn-bytes p2-wire "leaves" leaves))))))))

(deftest group-fields
  (let [grp (dyn-message grp-d "note" "g")
        f (fd p2-wire "grp")]
    (testing "singular: start and end tags"
      (let [bs (bytes-of (wire/message-writer 33 :singular true) grp)
            in (CodedInputStream/newInstance bs)]
        (is (= (wire/tag 33 WireFormat/WIRETYPE_START_GROUP) (.readTag in)))
        (is (= "g" (.getField ^Message (.getField (dyn-parse p2-wire bs) f) (fd grp-d "note"))))
        (is (= grp (read-one (wire/message-reader* 33 :singular true (parser-of grp-d))
                             (wire/tag 33 WireFormat/WIRETYPE_START_GROUP) bs)))))
    (testing "repeated groups"
      (let [grps (ArrayList. [(dyn-message grps-d "n" (int 1)) (dyn-message grps-d "n" (int 2))])
            f (fd p2-wire "grps")
            bs (bytes-of (wire/message-writer 34 :expanded true) grps)]
        (is (= (vec grps) (.getField (dyn-parse p2-wire bs) f)))
        (is (= (vec grps) (read-one (wire/message-reader* 34 :expanded true (parser-of grps-d))
                                    (wire/tag 34 WireFormat/WIRETYPE_START_GROUP)
                                    (dyn-bytes p2-wire "grps" grps))))))
    (testing "editions DELIMITED is the same encoding"
      (let [leaf-e (.getMessageType (fd e24-wire "delimited"))
            v (dyn-message leaf-e "id" "d")
            bs (bytes-of (wire/message-writer 5 :singular true) v)]
        (is (= "d" (.getField ^Message (.getField (dyn-parse e24-wire bs) (fd e24-wire "delimited")) (fd leaf-e "id"))))
        (is (java.util.Arrays/equals bs (dyn-bytes e24-wire "delimited" v)))))))

;; ---------------------------------------------------------------------------
;; Maps

(defn- entries
  "DynamicMessage's view of a map field: a list of entry messages."
  [^DynamicMessage m ^Descriptors$FieldDescriptor f]
  (let [ed (.getMessageType f)]
    (into {} (map (fn [^Message e] [(.getField e (fd ed "key")) (.getField e (fd ed "value"))]))
          (.getField m f))))

(defn- entry-messages [^Descriptors$FieldDescriptor f m]
  (let [ed (.getMessageType f)]
    (ArrayList. ^java.util.Collection
                (mapv (fn [[k v]] (dyn-message ed "key" k "value" v)) m))))

(deftest map-fields
  (testing "int32 keys, message values"
    (let [f (fd p2-wire "by_id")
          m (doto (LinkedHashMap.)
              (.put (int 2) (dyn-message leaf-d "id" "two"))
              (.put (int -1) (dyn-message leaf-d "id" "minus")))
          bs (bytes-of (wire/map-writer* 25 "INT32" "MESSAGE") m)]
      (is (= {2 (dyn-message leaf-d "id" "two") -1 (dyn-message leaf-d "id" "minus")}
             (entries (dyn-parse p2-wire bs) f)))
      (let [r (wire/map-reader* "INT32" {} (int 0)
                                "MESSAGE" {:parser (parser-of leaf-d)} (DynamicMessage/getDefaultInstance leaf-d))
            back (read-one r (wire/tag 25 WireFormat/WIRETYPE_LENGTH_DELIMITED)
                           (dyn-bytes p2-wire "by_id" (entry-messages f m)))]
        (is (instance? LinkedHashMap back))
        (is (= [2 -1] (vec (.keySet ^LinkedHashMap back))) "insertion order is wire order")
        (is (= m back)))))
  (testing "string keys, enum values, as numbers"
    (let [f (fd p2-wire "by_color")
          m (doto (LinkedHashMap.) (.put "a" (int 1)) (.put "b" (int 0)))
          bs (bytes-of (wire/map-writer* 26 "STRING" "ENUM") m)
          parsed (entries (dyn-parse p2-wire bs) f)]
      (is (= {"a" 1 "b" 0}
             (into {} (map (fn [[k ^Descriptors$EnumValueDescriptor v]] [k (.getNumber v)])) parsed))
          "a default value is written, not skipped")
      (let [et (.getEnumType (fd (.getMessageType f) "value"))
            evs (entry-messages f {"a" (.findValueByNumber et 1) "b" (.findValueByNumber et 0)})
            r (wire/map-reader* "STRING" {:utf8? true} "" "ENUM" {} (int 0))]
        (is (= m (read-one r (wire/tag 26 WireFormat/WIRETYPE_LENGTH_DELIMITED)
                           (dyn-bytes p2-wire "by_color" evs)))))))
  (testing "an entry missing its value reads the default"
    (let [only-key (let [out (ByteArrayOutputStream.) cos (CodedOutputStream/newInstance out)
                         entry (CodedOutputStream/computeStringSize 1 "k")]
                     (.writeTag cos 26 WireFormat/WIRETYPE_LENGTH_DELIMITED)
                     (.writeUInt32NoTag cos entry)
                     (.writeString cos 1 "k")
                     (.flush cos) (.toByteArray out))
          r (wire/map-reader* "STRING" {:utf8? true} "" "ENUM" {} (int 0))]
      (is (= {"k" 0} (read-one r (wire/tag 26 WireFormat/WIRETYPE_LENGTH_DELIMITED) only-key))))))
