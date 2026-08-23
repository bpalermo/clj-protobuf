(ns clj-protobuf.impl.naming
  "The one naming rule, shared with the emitter.

  protoc-gen-clojure kebab-cases proto field names into record fields and map
  keys with exactly this algorithm (its `field-key-symbol`). The runtime's
  generic nested-map path must produce the same keys byte for byte, or records
  built by generated code and maps built by the runtime stop being
  interchangeable. Any change here is a wire-compatibility break with every
  generated file in existence — don't.

  Kebab-casing is lossy (STYLE_LEGACY files can mix conventions), which is why
  the emitted `rt/field` lookups carry the exact proto name and this fn is used
  only for the Clojure-side keys."
  (:require [clojure.string :as str]))

(defn field-key
  "proto field name -> keyword. camelCaseField -> :camel-case-field,
  UPPER_SNAKE_FIELD -> :upper-snake-field, already_snake -> :already-snake."
  [s]
  (keyword
   (-> s
       (str/replace #"([a-z0-9])([A-Z])" "$1-$2")
       (str/replace "_" "-")
       (str/lower-case))))
