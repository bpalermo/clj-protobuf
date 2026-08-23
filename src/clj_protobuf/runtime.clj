(ns clj-protobuf.runtime
  "The runtime half of protoc-gen-clojure's generated-code contract.

  Generated files call exactly four things here: `file-descriptor` to rebuild
  their embedded FileDescriptorProto, `known-file` for well-known-type deps,
  `message` for a prototype per message, and `field` for a handle per field.
  Everything protobuf decides per edition — presence, delimited encoding, utf8
  validation, packedness — is resolved by protobuf-java when the descriptor is
  built, which is why generated code never mentions editions at all.

  `field` returns a precomputed FieldHandle rather than a bare FieldDescriptor:
  the codec's hot path dispatches on a keyword and never touches the descriptor
  API per call. Handles are built against a specific prototype, so a
  message-typed field's nested prototype has the right concrete class in both
  the DynamicMessage and generated-class arms."
  (:require [clj-protobuf.impl.naming :as naming]
            [clojure.string :as str])
  (:import [com.google.protobuf
            DescriptorProtos$FileDescriptorProto
            Descriptors$Descriptor
            Descriptors$EnumDescriptor
            Descriptors$EnumValueDescriptor
            Descriptors$FieldDescriptor
            Descriptors$FieldDescriptor$JavaType
            Descriptors$FileDescriptor
            DynamicMessage
            Message
            Message$Builder]
           [java.util Base64]))

(set! *warn-on-reflection* true)

(defn file-descriptor
  "Build a FileDescriptor from the base64 FileDescriptorProto a generated file
  embeds, linked against its dependencies (already-built FileDescriptors —
  sibling namespaces' `file-descriptor` vars or `known-file` results)."
  ^Descriptors$FileDescriptor [^String descriptor-b64 deps]
  (try
    (let [bytes (.decode (Base64/getDecoder) descriptor-b64)
          fdp   (DescriptorProtos$FileDescriptorProto/parseFrom ^bytes bytes)]
      (Descriptors$FileDescriptor/buildFrom
       fdp (into-array Descriptors$FileDescriptor deps)))
    (catch Exception e
      (throw (ex-info "failed to build FileDescriptor from embedded descriptor"
                      {:clj-protobuf/error :descriptor}
                      e)))))

;; Well-known types ship inside protobuf-java with their descriptors already
;; built; `known-file` hands the right one back by path. Only google/protobuf/*
;; qualifies — google/rpc and google/api live in separate artifacts, and the
;; emitter never asks for them.
(def ^:private known-files
  (delay
    (into {}
          (map (fn [^Descriptors$FileDescriptor fd] [(.getName fd) fd]))
          [(.getFile (com.google.protobuf.AnyProto/getDescriptor))
           (.getFile (com.google.protobuf.ApiProto/getDescriptor))
           (.getFile (com.google.protobuf.DurationProto/getDescriptor))
           (.getFile (com.google.protobuf.EmptyProto/getDescriptor))
           (.getFile (com.google.protobuf.FieldMaskProto/getDescriptor))
           (.getFile (com.google.protobuf.SourceContextProto/getDescriptor))
           (.getFile (com.google.protobuf.StructProto/getDescriptor))
           (.getFile (com.google.protobuf.TimestampProto/getDescriptor))
           (.getFile (com.google.protobuf.TypeProto/getDescriptor))
           (.getFile (com.google.protobuf.WrappersProto/getDescriptor))
           (.getFile (com.google.protobuf.DescriptorProtos/getDescriptor))])))

(defn known-file
  "The FileDescriptor for a well-known type bundled in protobuf-java, e.g.
  \"google/protobuf/timestamp.proto\"."
  ^Descriptors$FileDescriptor [^String path]
  (or (get @known-files path)
      (throw (ex-info (str "not a well-known protobuf file: " path)
                      {:clj-protobuf/error :no-such-type
                       :path path}))))

(defn- resolve-message-type
  "Walk a dotted lookup name to a Descriptor. FileDescriptor.findMessageTypeByName
  resolves only top-level names — every nested spelling returns nil — so nested
  types are walked one segment at a time via Descriptor.findNestedTypeByName."
  ^Descriptors$Descriptor [^Descriptors$FileDescriptor fd ^String lookup]
  (let [[head & tail] (str/split lookup #"\.")]
    (reduce (fn [^Descriptors$Descriptor d segment]
              (if d
                (.findNestedTypeByName d ^String segment)
                (reduced nil)))
            (.findMessageTypeByName fd ^String head)
            tail)))

(defn- hinted-default-instance
  "Try the emitted Java class hint. The hint is only ever a hint: any failure —
  class absent, no getDefaultInstance, or the class describing a different
  message — silently yields nil and the caller keeps its DynamicMessage. Being
  wrong costs the optimisation, never the bytes."
  ^Message [^String class-name ^Descriptors$Descriptor descriptor]
  (try
    (let [cls (Class/forName class-name)
          m   (.getMethod cls "getDefaultInstance" (make-array Class 0))
          inst ^Message (.invoke m nil (make-array Object 0))]
      (when (= (.getFullName (.getDescriptorForType inst))
               (.getFullName descriptor))
        inst))
    (catch Throwable _ nil)))

(defn message
  "The prototype for a message type: a default instance whose
  `.newBuilderForType` the generated `->proto` fns drive.

  With a Java class hint (3-arity) the generated class's default instance is
  used when it is present and describes the same message — measured ~45% faster
  to encode and ~46% lighter on allocation than DynamicMessage for small
  messages. Otherwise, and always in the 2-arity, a DynamicMessage prototype.
  Same codec, same field descriptors, same bytes either way."
  (^Message [fd lookup] (message fd lookup nil))
  (^Message [^Descriptors$FileDescriptor fd ^String lookup class-hint]
   (let [descriptor (or (resolve-message-type fd lookup)
                        (throw (ex-info (str "no message type " lookup
                                             " in " (.getName fd))
                                        {:clj-protobuf/error :no-such-type
                                         :lookup lookup
                                         :file (.getName fd)})))]
     (or (when class-hint (hinted-default-instance class-hint descriptor))
         (DynamicMessage/getDefaultInstance descriptor)))))

;; ---------------------------------------------------------------------------
;; Field handles

(defrecord FieldHandle
           [^Descriptors$FieldDescriptor fd
            kind          ; :int :long :float :double :boolean :string :bytes :enum :message
            repeated?     ; non-map repeated
            map?
            has-presence?
            ^Descriptors$EnumDescriptor enum-type
            key-handle    ; map entry key FieldHandle
            val-handle    ; map entry value FieldHandle
            ^Message nested-prototype ; message kind: the field's message default,
                                      ; concrete-class-correct for this lineage;
                                      ; for maps, the entry prototype
            children      ; delay of [[kebab-kw FieldHandle] ...] (message kind)
            kebab-key
            proto-key
            enum-kw])     ; {EnumValueDescriptor -> keyword}, enum kind only

(defn- kind-of [^Descriptors$FieldDescriptor fd]
  (condp = (.getJavaType fd)
    Descriptors$FieldDescriptor$JavaType/INT         :int
    Descriptors$FieldDescriptor$JavaType/LONG        :long
    Descriptors$FieldDescriptor$JavaType/FLOAT       :float
    Descriptors$FieldDescriptor$JavaType/DOUBLE      :double
    Descriptors$FieldDescriptor$JavaType/BOOLEAN     :boolean
    Descriptors$FieldDescriptor$JavaType/STRING      :string
    Descriptors$FieldDescriptor$JavaType/BYTE_STRING :bytes
    Descriptors$FieldDescriptor$JavaType/ENUM        :enum
    Descriptors$FieldDescriptor$JavaType/MESSAGE     :message))

(defn- make-handle
  ^clj_protobuf.runtime.FieldHandle
  [^Message prototype ^Descriptors$FieldDescriptor fd]
  (let [kind      (kind-of fd)
        map-field (.isMapField fd)
        repeated  (and (.isRepeated fd) (not map-field))
        ;; The nested prototype comes from the parent builder so it has the
        ;; right concrete class: a generated parent yields the generated nested
        ;; class, a DynamicMessage parent yields DynamicMessage. For maps this
        ;; is the entry prototype.
        nested    (when (= kind :message)
                    (-> (.newBuilderForType prototype)
                        (.newBuilderForField fd)
                        (.getDefaultInstanceForType)))
        [kh vh]   (when map-field
                    (let [ed (.getMessageType fd)]
                      [(make-handle nested (.findFieldByName ed "key"))
                       (make-handle nested (.findFieldByName ed "value"))]))
        children  (when (and (= kind :message) (not map-field))
                    ;; Delayed: descriptors can be cyclic (a message containing
                    ;; itself), and an eager walk would never terminate.
                    (delay
                      (mapv (fn [^Descriptors$FieldDescriptor cfd]
                              [(naming/field-key (.getName cfd))
                               (make-handle nested cfd)])
                            (.getFields (.getDescriptorForType nested)))))]
    (->FieldHandle fd kind repeated map-field (.hasPresence fd)
                   (when (= kind :enum) (.getEnumType fd))
                   kh vh nested children
                   (naming/field-key (.getName fd))
                   (keyword (.getName fd))
                   ;; Interning a keyword per read is measurable on enum-heavy
                   ;; messages; the value set is small and known now.
                   (when (= kind :enum)
                     (into {}
                           (map (fn [^Descriptors$EnumValueDescriptor v]
                                  [v (keyword (.getName v))]))
                           (.getValues (.getEnumType fd)))))))

(defn field
  "A precomputed handle for one field of a message prototype, looked up by its
  exact proto field name — the name is the authority; kebab-cased keys are
  derived from it, never the reverse (STYLE_LEGACY files can mix conventions)."
  [^Message prototype ^String field-name]
  (let [descriptor (.getDescriptorForType prototype)
        fd         (.findFieldByName descriptor field-name)]
    (when (nil? fd)
      (throw (ex-info (str "no field " field-name " in "
                           (.getFullName descriptor))
                      {:clj-protobuf/error :no-such-field
                       :field field-name
                       :message (.getFullName descriptor)})))
    (make-handle prototype fd)))
