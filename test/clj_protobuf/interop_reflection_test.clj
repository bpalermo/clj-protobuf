(ns clj-protobuf.interop-reflection-test
  "interop=true output must compile without reflection warnings — its whole
  premise is direct typed calls, and one reflective setter costs more than the
  codec path it replaces (measured ~7 µs and 12 KB per nested message).

  Recompiles the interop fixtures from source with *warn-on-reflection* bound.
  This target's classpath carries the source variant of those fixtures so
  there is something to recompile; the runtime they require is loaded as-is."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [java.io StringWriter]))

(deftest interop-fixtures-compile-reflection-free
  (let [err (StringWriter.)]
    (binding [*err* err
              *warn-on-reflection* true]
      (require 'interop.fixtures.bench.shapes
               'interop.fixtures.e2024.kitchen
               :reload))
    (let [warnings (->> (str/split-lines (str err))
                        (filter #(str/includes? % "Reflection warning")))]
      (is (empty? warnings)
          (str "reflection warnings in interop fixtures:\n"
               (str/join "\n" warnings))))))
