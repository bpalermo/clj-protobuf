(ns clj-protobuf.invoker-test
  "The typed-accessor invokers: present exactly where they should be, absent
  exactly where they must be, and interchangeable with the reflection path.
  This target has the generated Java classes on its classpath, so the fixture
  prototypes are hinted."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.codec :as codec]
            [clj-protobuf.core :as pb]
            [clj-protobuf.impl.invoke :as invoke]
            [clj-protobuf.runtime :as rt]
            [fixtures.e2024.kitchen :as e2024]
            [fixtures.p2.kitchen :as p2]))

(deftest accessor-suffix-rules
  (is (= "RepeatCount" (invoke/accessor-suffix "repeat_count")))
  (is (= "F10" (invoke/accessor-suffix "f10")))
  (is (= "CamelCaseField" (invoke/accessor-suffix "camelCaseField")))
  (is (= "X" (invoke/accessor-suffix "x")))
  (is (= "AlreadySnake" (invoke/accessor-suffix "already_snake"))))

(defn- handle [proto-name]
  (rt/field e2024/Kitchen-prototype proto-name))

(deftest invokers-present-on-hinted-singular-fields
  (doseq [f ["str_field" "int_field" "bool_field" "bytes_field"
             "dbl_field" "long_field" "msg_field"]]
    (testing f
      (let [h (handle f)]
        (is (some? (:set-invoker h)))
        (is (some? (:get-invoker h)))
        (is (nil? (:clear-invoker h)))))))

(deftest has-invoker-only-where-presence-exists
  (is (some? (:has-invoker (handle "str_field"))))
  (testing "IMPLICIT presence has no hasX accessor and no absence to report"
    (is (nil? (:has-invoker (handle "implicit_field"))))))

(deftest collection-invokers-come-with-clear
  (testing "repeated and map fields get the bulk accessors plus clearX, so
            set-field! can keep replace semantics over append/merge accessors"
    (doseq [f ["tags" "children" "counts"]]
      (testing f
        (let [h (handle f)]
          (is (some? (:set-invoker h)))
          (is (some? (:get-invoker h)))
          (is (some? (:clear-invoker h)))
          (is (nil? (:has-invoker h))))))))

(deftest enum-invokers-only-for-open-enums
  (testing "editions enums are open: setXValue/getXValue exist"
    (let [h (handle "enum_field")]
      (is (some? (:set-invoker h)))
      (is (some? (:get-invoker h)))
      (is (some? (:has-invoker h)))))
  (testing "proto2 enums are closed: no number accessors, reflection path"
    (let [h (rt/field p2/Kitchen-prototype "enum_field")]
      (is (not (instance? com.google.protobuf.DynamicMessage p2/Kitchen-prototype)))
      (is (nil? (:set-invoker h)))
      (is (nil? (:get-invoker h)))
      (is (some? (:has-invoker h))))))

(deftest no-invokers-on-the-dynamic-arm
  (let [proto (rt/message e2024/file-descriptor "Kitchen")]
    (doseq [f ["str_field" "tags" "counts" "enum_field"]]
      (let [h (rt/field proto f)]
        (is (nil? (:set-invoker h)))
        (is (nil? (:get-invoker h)))
        (is (nil? (:has-invoker h)))
        (is (nil? (:clear-invoker h)))))))

(deftest underivable-accessors-fall-back-silently
  (is (nil? (invoke/setter-invoker (class (.newBuilderForType e2024/Kitchen-prototype))
                                   "setNoSuchAccessor" String)))
  (is (nil? (invoke/getter-invoker (class e2024/Kitchen-prototype)
                                   "getNoSuchAccessor" String))))

(deftest invoker-and-reflection-arms-agree-on-presence-and-values
  (testing "zero values with explicit presence survive the invoker path"
    (let [rec (e2024/map->Kitchen {:int-field 0 :bool-field false :str-field ""})
          out (->> (e2024/Kitchen->proto rec)
                   pb/encode
                   (pb/decode e2024/Kitchen-prototype)
                   e2024/proto->Kitchen)]
      (is (= 0 (:int-field out)))
      (is (= false (:bool-field out)))
      (is (= "" (:str-field out)))
      (is (nil? (:dbl-field out))))))

(defn- round-trip [m]
  (->> (e2024/Kitchen->proto m)
       pb/encode
       (pb/decode e2024/Kitchen-prototype)
       e2024/proto->Kitchen))

(deftest collections-round-trip-through-invokers
  (let [out (round-trip {:tags ["a" "b" "c"]
                         :children [{:id "c1"} {:id "c2"}]
                         :counts {"x" 1 "y" 2}})]
    (is (= ["a" "b" "c"] (:tags out)))
    (is (= [{:id "c1"} {:id "c2"}] (:children out)))
    (is (= {"x" 1 "y" 2} (:counts out))))
  (testing "empty collections are absent, as on the reflection path"
    (let [out (round-trip {:tags [] :counts {}})]
      (is (nil? (:tags out)))
      (is (nil? (:counts out))))))

(deftest set-field-replaces-collections-on-a-reused-builder
  (testing "addAllX appends and putAllX merges; set-field! must still replace"
    (let [b (.newBuilderForType ^com.google.protobuf.Message e2024/Kitchen-prototype)]
      (codec/set-field! b (handle "tags") ["a" "b"] nil)
      (codec/set-field! b (handle "tags") ["c"] nil)
      (codec/set-field! b (handle "counts") {"x" 1 "y" 2} nil)
      (codec/set-field! b (handle "counts") {"z" 3} nil)
      (let [out (e2024/proto->Kitchen (.build b))]
        (is (= ["c"] (:tags out)))
        (is (= {"z" 3} (:counts out)))))))

(deftest enums-round-trip-through-number-invokers
  (is (= :COLOR_BLUE (:enum-field (round-trip {:enum-field :COLOR_BLUE}))))
  (is (= :COLOR_RED (:enum-field (round-trip {:enum-field "COLOR_RED"}))))
  (is (= :COLOR_RED (:enum-field (round-trip {:enum-field 1}))))
  (testing "the zero value with explicit presence is present, not absent"
    (is (= :COLOR_UNSPECIFIED (:enum-field (round-trip {:enum-field 0}))))
    (is (nil? (:enum-field (round-trip {})))))
  (testing "open enum: an undeclared number survives as a synthesized value"
    (let [msg (pb/decode e2024/Kitchen-prototype
                         (pb/encode (e2024/Kitchen->proto {:enum-field 42})))]
      (is (= 42 (codec/get-field msg (handle "enum_field") {:enums :number})))
      (is (= 42 (:enum-field (e2024/proto->Kitchen msg {:enums :number}))))))
  (testing "representation opts still apply on the invoker path"
    (let [msg (e2024/Kitchen->proto {:enum-field :COLOR_BLUE})]
      (is (= 2 (codec/get-field msg (handle "enum_field") {:enums :number})))
      (is (= "COLOR_BLUE" (codec/get-field msg (handle "enum_field") {:enums :string})))))
  (testing "an unknown name is a type mismatch, as before"
    (is (thrown? clojure.lang.ExceptionInfo
                 (e2024/Kitchen->proto {:enum-field :COLOR_PLAID})))))

(deftest prototypes-from-descriptors-take-the-hinted-arm
  (testing "with the generated classes on the classpath, a bare Descriptor — what a
            gRPC marshaller holds — resolves to the generated class, the arm the
            namespace's proto->X fns read, without the caller knowing protoc's
            naming rules"
    (doseq [[proto cls] [[e2024/Kitchen-prototype com.acme.fixtures.e2024.Kitchen]
                         [e2024/NestedInFileClass-prototype com.acme.fixtures.e2024.KitchenProto$NestedInFileClass]
                         [p2/Kitchen-prototype com.acme.fixtures.p2.Kitchen]]]
      (let [d (.getDescriptorForType ^com.google.protobuf.Message proto)]
        (is (instance? cls (rt/prototype d)))
        (is (identical? (class proto) (class (rt/prototype d)))))))
  (testing "a DynamicMessage built the old way is re-resolved onto the generated class"
    (is (instance? com.acme.fixtures.e2024.Kitchen
                   (rt/prototype (rt/dynamic-message e2024/file-descriptor "Kitchen"))))))
