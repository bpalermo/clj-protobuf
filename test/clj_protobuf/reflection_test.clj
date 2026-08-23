(ns clj-protobuf.reflection-test
  "The library must compile without reflection warnings — reflective interop on
  the codec hot path silently costs an order of magnitude.

  Recompiles every namespace from source with *warn-on-reflection* bound; the
  target's classpath deliberately carries the source variant of the library so
  there are sources to recompile."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [java.io StringWriter]))

(deftest no-reflection-warnings
  (let [err (StringWriter.)]
    (binding [*err* err
              *warn-on-reflection* true]
      (require 'clj-protobuf.impl.naming
               'clj-protobuf.runtime
               'clj-protobuf.codec
               'clj-protobuf.core
               'clj-grpc.runtime
               'clj-grpc.codec
               :reload-all))
    (let [warnings (->> (str/split-lines (str err))
                        (filter #(str/includes? % "Reflection warning")))]
      (is (empty? warnings)
          (str "reflection warnings:\n" (str/join "\n" warnings))))))
