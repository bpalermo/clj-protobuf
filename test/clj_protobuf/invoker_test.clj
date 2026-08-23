(ns clj-protobuf.invoker-test
  "The typed-accessor invokers: present exactly where they should be, absent
  exactly where they must be, and interchangeable with the reflection path.
  This target has the generated Java classes on its classpath, so the fixture
  prototypes are hinted."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.core :as pb]
            [clj-protobuf.impl.invoke :as invoke]
            [clj-protobuf.runtime :as rt]
            [fixtures.e2024.kitchen :as e2024]))

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
        (is (some? (:get-invoker h)))))))

(deftest has-invoker-only-where-presence-exists
  (is (some? (:has-invoker (handle "str_field"))))
  (testing "IMPLICIT presence has no hasX accessor and no absence to report"
    (is (nil? (:has-invoker (handle "implicit_field"))))))

(deftest no-invokers-where-the-reflection-path-owns-the-field
  (doseq [f ["enum_field" "tags" "children" "counts"]]
    (testing f
      (let [h (handle f)]
        (is (nil? (:set-invoker h)))
        (is (nil? (:get-invoker h)))))))

(deftest no-invokers-on-the-dynamic-arm
  (let [proto (rt/message e2024/file-descriptor "Kitchen")
        h (rt/field proto "str_field")]
    (is (nil? (:set-invoker h)))
    (is (nil? (:get-invoker h)))
    (is (nil? (:has-invoker h)))))

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
