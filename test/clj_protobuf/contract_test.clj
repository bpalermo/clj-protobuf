(ns clj-protobuf.contract-test
  "The generated-code contract, exercised through real generated fixtures.

  These tests are the suite protoc-gen-clojure explicitly delegates: whether
  the emitted code works — loads, round-trips on the wire, keeps records and
  plain maps interchangeable — is asserted here, against the runtime the
  generated code requires."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.core :as pb]
            [fixtures.e2023.kitchen]
            [fixtures.e2024.kitchen :as e2024]
            [fixtures.e2024legacy.legacy-style :as legacy]
            [fixtures.nested.nested :as nested]
            [fixtures.p2.kitchen]
            [fixtures.p3.kitchen :as p3]
            [fixtures.bench.shapes]))

(defn- normalize
  "Byte arrays compare by identity; replace them with seqs so records built
  before and after a round trip compare with =."
  [rec]
  (if (some? (:bytes-field rec))
    (update rec :bytes-field seq)
    rec))

(defn- kitchen-round-trip [rec]
  (->> (e2024/Kitchen->proto rec)
       pb/encode
       (pb/decode e2024/Kitchen-prototype)
       e2024/proto->Kitchen))

(def full-kitchen
  (e2024/map->Kitchen
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
    :choice-str nil
    :choice-int 7
    :choice-msg nil
    ;; The well-known types are proto3 files: every scalar has IMPLICIT
    ;; presence, so a nested map read back always carries the defaults too.
    ;; Writing them here keeps the round trip an identity.
    :ts {:seconds 5 :nanos 100}
    :dur {:seconds 30 :nanos 0}
    :wrapped {:value "wrapped"}
    :implicit-field "imp"
    :delimited {:note "grouped"}}))

(deftest full-round-trip
  (let [out (kitchen-round-trip full-kitchen)]
    (is (= (normalize full-kitchen) (normalize out)))))

(deftest empty-round-trip
  (testing "every absent field is nil, except IMPLICIT presence which has no
            absence and reports its default"
    (let [out (kitchen-round-trip (e2024/map->Kitchen {}))]
      (is (= (assoc (e2024/map->Kitchen {}) :implicit-field "")
             out)))))

(deftest records-and-maps-interchangeable
  (let [as-record (e2024/Kitchen->proto full-kitchen)
        as-map    (e2024/Kitchen->proto (into {} full-kitchen))]
    (is (java.util.Arrays/equals (pb/encode as-record) (pb/encode as-map))))
  (testing "nested values accept the nested record type too"
    (let [with-rec (e2024/Kitchen->proto {:msg-field (e2024/->Nested "n")})
          with-map (e2024/Kitchen->proto {:msg-field {:id "n"}})]
      (is (java.util.Arrays/equals (pb/encode with-rec) (pb/encode with-map))))))

(deftest nested-messages-read-back-as-maps
  (let [out (kitchen-round-trip full-kitchen)]
    (is (= {:id "nested"} (:msg-field out)))
    (is (= [{:id "c1"} {:id "c2"}] (:children out)))))

(deftest oneof-last-set-wins
  (testing "generated code sets oneof members in declaration order; the builder
            keeps the last one, exactly like protobuf"
    (let [out (kitchen-round-trip (e2024/map->Kitchen {:choice-str "s" :choice-int 9}))]
      (is (nil? (:choice-str out)))
      (is (= 9 (:choice-int out))))
    (let [out (kitchen-round-trip (e2024/map->Kitchen {:choice-int 9 :choice-msg {:id "m"}}))]
      (is (nil? (:choice-int out)))
      (is (= {:id "m"} (:choice-msg out))))))

(deftest enum-representation
  (let [out (kitchen-round-trip (e2024/map->Kitchen {:enum-field :COLOR_BLUE}))]
    (is (= :COLOR_BLUE (:enum-field out)))))

(deftest map-field
  (let [out (kitchen-round-trip (e2024/map->Kitchen {:counts {"a" 1 "b" 2 "c" 3}}))]
    (is (= {"a" 1 "b" 2 "c" 3} (:counts out)))))

(deftest int64-precision
  (testing "int64 survives beyond double precision"
    (let [out (kitchen-round-trip (e2024/map->Kitchen {:long-field Long/MAX_VALUE}))]
      (is (= Long/MAX_VALUE (:long-field out))))))

(deftest delimited-encoding-round-trips
  (testing "editions DELIMITED (group-style wire encoding) flows through the
            descriptor with no special handling"
    (let [out (kitchen-round-trip (e2024/map->Kitchen {:delimited {:note "d"}}))]
      (is (= {:note "d"} (:delimited out))))))

(deftest style-legacy-proto-names-are-authoritative
  (testing "STYLE_LEGACY fields keep their unconventional proto names; the
            kebab keys still work because rt/field carries the exact name"
    (let [rec (legacy/map->legacyStyleMessage
               {:camel-case-field "c" :upper-snake-field "u"
                :already-snake "a" :x "x" :http2-server "h"})
          out (->> (legacy/legacyStyleMessage->proto rec)
                   pb/encode
                   (pb/decode legacy/legacyStyleMessage-prototype)
                   legacy/proto->legacyStyleMessage)]
      (is (= rec out)))))

(deftest nested-types-resolve-dotted
  (testing "Outer.Inner and deeper resolve through the dotted lookup"
    (let [rec (nested/map->Outer
               {:id "o"
                :counts {"k" 1}
                :inner {:name "i" :labels {"env" "prod"}
                        :innermost {:depth 3}}})
          out (->> (nested/Outer->proto rec)
                   pb/encode
                   (pb/decode nested/Outer-prototype)
                   nested/proto->Outer)]
      (is (= rec out)))))

(deftest proto3-optional-presence
  (testing "proto3 optional scalars are nil when unset, value when set —
            including the zero value"
    (let [round (fn [m] (->> (p3/Kitchen->proto m)
                             pb/encode
                             (pb/decode p3/Kitchen-prototype)
                             p3/proto->Kitchen))]
      (is (nil? (:opt-str (round {}))))
      (is (= "" (:opt-str (round {:opt-str ""}))))
      (is (= 0 (:opt-int (round {:opt-int 0})))))))

(deftest all-fixture-namespaces-load
  (doseq [sym '[fixtures.p2.kitchen fixtures.p3.kitchen fixtures.e2023.kitchen
                fixtures.e2024.kitchen fixtures.e2024legacy.legacy-style
                fixtures.nested.nested fixtures.bench.shapes]]
    (is (some? (find-ns sym)) (str sym " loaded"))))
