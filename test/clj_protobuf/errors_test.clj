(ns clj-protobuf.errors-test
  "Every failure is an ex-info carrying {:clj-protobuf/error <category>}, so
  callers can dispatch without string-matching messages."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.codec :as codec]
            [clj-protobuf.core :as pb]
            [clj-protobuf.runtime :as rt]
            [fixtures.e2024.kitchen :as e2024])
  (:import [com.google.protobuf Message]))

(defn- error-category [f]
  (try
    (f)
    ::no-throw
    (catch clojure.lang.ExceptionInfo e
      (:clj-protobuf/error (ex-data e)))))

(deftest no-such-field
  (is (= :no-such-field
         (error-category #(rt/field e2024/Kitchen-prototype "no_such")))))

(deftest no-such-type
  (is (= :no-such-type
         (error-category #(rt/message e2024/file-descriptor "NoSuch"))))
  (is (= :no-such-type
         (error-category #(rt/message e2024/file-descriptor "Kitchen.NoSuch"))))
  (is (= :no-such-type
         (error-category #(rt/known-file "google/rpc/status.proto")))))

(deftest type-mismatch
  (let [b (.newBuilderForType ^Message e2024/Kitchen-prototype)
        h #(rt/field e2024/Kitchen-prototype %)]
    (testing "wrong value shapes name the field and what it wanted"
      (is (= :type-mismatch (error-category #(codec/set-field! b (h "str_field") 42 nil))))
      (is (= :type-mismatch (error-category #(codec/set-field! b (h "int_field") "x" nil))))
      (is (= :type-mismatch (error-category #(codec/set-field! b (h "bool_field") "true" nil))))
      (is (= :type-mismatch (error-category #(codec/set-field! b (h "enum_field") :NOT_A_COLOR nil))))
      (is (= :type-mismatch (error-category #(codec/set-field! b (h "tags") "not-a-vector" nil))))
      (is (= :type-mismatch (error-category #(codec/set-field! b (h "counts") [["x" 1]] nil))))
      (is (= :type-mismatch (error-category #(codec/set-field! b (h "msg_field") "nope" nil)))))
    (testing "the ex-data names the field"
      (try
        (codec/set-field! b (h "str_field") 42 nil)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= "fixtures.e2024.Kitchen.str_field" (:field (ex-data e)))))))))

(deftest parse-errors
  (is (= :parse
         (error-category #(pb/decode e2024/Kitchen-prototype
                                     (byte-array [-1 -1 -1 -1 -1])))))
  (is (= :parse
         (error-category #(pb/decode e2024/Kitchen-prototype "not bytes")))))

(deftest descriptor-errors
  (is (= :descriptor
         (error-category #(rt/file-descriptor "!!!not base64!!!" []))))
  (is (= :descriptor
         (error-category
          #(rt/file-descriptor
            (.encodeToString (java.util.Base64/getEncoder)
                             (byte-array [-1 -2 -3]))
            [])))))

(deftest hint-failures-are-silent
  (testing "a wrong class hint costs the optimisation, never an error"
    (let [via-bogus  (rt/message e2024/file-descriptor "Kitchen" "no.such.Class")
          via-wrong  (rt/message e2024/file-descriptor "Kitchen" "java.lang.String")
          plain      (rt/message e2024/file-descriptor "Kitchen")]
      (is (instance? com.google.protobuf.DynamicMessage via-bogus))
      (is (instance? com.google.protobuf.DynamicMessage via-wrong))
      (is (instance? com.google.protobuf.DynamicMessage plain)))))
