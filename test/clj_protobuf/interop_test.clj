(ns clj-protobuf.interop-test
  "interop=true output against the codec arm and against protoc: same bytes,
  same semantics, from real generated code — the validation the plugin
  delegates here."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.core :as pb]
            [fixtures.e2024.kitchen :as std]
            [interop.fixtures.e2024.kitchen :as fast]
            [interop.fixtures.bench.shapes :as fast-shapes]))

(def kitchen-value
  {:str-field "hello" :int-field 42 :bool-field true
   :bytes-field (byte-array [1 2 3]) :dbl-field 3.5
   :long-field 9007199254740993 :enum-field :COLOR_RED
   :msg-field {:id "nested"} :tags ["a" "b"]
   :children [{:id "c1"} {:id "c2"}] :counts {"x" 1 "y" 2}
   :choice-int 7 :ts {:seconds 5 :nanos 100} :dur {:seconds 30}
   :wrapped {:value "wrapped"} :implicit-field "imp"
   :delimited {:note "grouped"}})

(deftest fast-arm-bytes-equal-codec-arm
  (testing "nil-opts (fast) and the standard namespace agree byte for byte"
    (is (java.util.Arrays/equals
         (pb/encode (std/Kitchen->proto kitchen-value))
         (pb/encode (fast/Kitchen->proto kitchen-value)))))
  (testing "an opts value routes the interop namespace onto the codec arm,
            same bytes again"
    (is (java.util.Arrays/equals
         (pb/encode (fast/Kitchen->proto kitchen-value nil))
         (pb/encode (fast/Kitchen->proto kitchen-value {:enums :keyword}))))))

(deftest fast-arm-nil-semantics
  (testing "nil fields set nothing; zero values with presence survive"
    (let [out (->> (fast/Kitchen->proto {:int-field 0 :str-field ""})
                   pb/encode
                   (pb/decode fast/Kitchen-prototype)
                   fast/proto->Kitchen)]
      (is (= 0 (:int-field out)))
      (is (= "" (:str-field out)))
      (is (nil? (:dbl-field out)))
      (is (nil? (:msg-field out))))))

(deftest fast-arm-message-values
  (testing "records, maps, and pre-built Messages all land identically"
    (let [via-map (fast/Kitchen->proto {:msg-field {:id "n"}})
          via-rec (fast/Kitchen->proto {:msg-field (fast/->Nested "n")})
          via-msg (fast/Kitchen->proto {:msg-field (fast/Nested->proto {:id "n"})})]
      (is (java.util.Arrays/equals (pb/encode via-map) (pb/encode via-rec)))
      (is (java.util.Arrays/equals (pb/encode via-map) (pb/encode via-msg))))))

(deftest shapes-load-and-agree
  (doseq [[to value]
          [[#'fast-shapes/Tiny->proto {:id "t" :n 1 :ok true}]
           [#'fast-shapes/Flat->proto {:f1 "a" :f4 7 :f6 1 :f8 true :f10 1.5}]]]
    (is (some? (pb/encode (to value))))))
