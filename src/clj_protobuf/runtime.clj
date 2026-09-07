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
  (:require [clj-protobuf.impl.compile :as compile]
            [clj-protobuf.impl.invoke :as invoke]
            [clj-protobuf.impl.message :as message]
            [clj-protobuf.impl.naming :as naming]
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
            GeneratedMessage
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

;; The kill switch: -Dclj-protobuf.codec=dynamic keeps DynamicMessage as the
;; non-generated prototype. A system property rather than a Var, because
;; rt/message runs when a generated namespace loads — under AOT, or inside a
;; native image — where binding a Var first is impractical, and because it
;; makes an A/B a one-line environment change. Read once.
(def ^:private compiled-codec?
  (not= "dynamic" (System/getProperty "clj-protobuf.codec")))

(defn- resolve-descriptor
  ^Descriptors$Descriptor [^Descriptors$FileDescriptor fd ^String lookup]
  (or (resolve-message-type fd lookup)
      (throw (ex-info (str "no message type " lookup
                           " in " (.getName fd))
                      {:clj-protobuf/error :no-such-type
                       :lookup lookup
                       :file (.getName fd)}))))

(defn dynamic-message
  "A DynamicMessage prototype, whatever the codec setting: the reference
  arm, for tests and comparisons."
  ^Message [^Descriptors$FileDescriptor fd ^String lookup]
  (DynamicMessage/getDefaultInstance (resolve-descriptor fd lookup)))

(defn compiled-message
  "A compiled-codec prototype, whatever the codec setting."
  ^Message [^Descriptors$FileDescriptor fd ^String lookup]
  (message/prototype (resolve-descriptor fd lookup)))

(defn message
  "The prototype for a message type: a default instance whose
  `.newBuilderForType` the generated `->proto` fns drive.

  With a Java class hint (3-arity) the generated class's default instance is
  used when it is present and describes the same message — protoc's own
  serializer, and the fastest arm. Otherwise, and always in the 2-arity, the
  compiled codec's prototype: a Message over a slot array with the
  descriptor compiled once into reader and writer tables. DynamicMessage
  serves only for extendable types, which the compiled codec does not
  support, or when -Dclj-protobuf.codec=dynamic asks for it. Same codec,
  same field descriptors, same bytes on every arm."
  (^Message [fd lookup] (message fd lookup nil))
  (^Message [^Descriptors$FileDescriptor fd ^String lookup class-hint]
   (let [descriptor (resolve-descriptor fd lookup)]
     (or (when class-hint (hinted-default-instance class-hint descriptor))
         (if (and compiled-codec? (not (.isExtendable descriptor)))
           (message/prototype descriptor)
           (DynamicMessage/getDefaultInstance descriptor))))))

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
            children      ; delay of Object[] of child FieldHandles (message kind)
            kebab-key
            proto-key
            enum-kw       ; {EnumValueDescriptor -> keyword}, enum kind only
            enum-by-kw    ; {keyword -> EnumValueDescriptor}, enum kind only
            enum-by-number ; {long -> EnumValueDescriptor}, enum kind only
            set-invoker   ; BiFunction over the typed setter, hinted arm only:
                          ; setX for singular, addAllX / putAllX for collections
            get-invoker   ; Function over the typed getter, hinted arm only:
                          ; getX, getXList, getXMap
            has-invoker   ; Function over hasX(), presence fields, hinted arm
            clear-invoker ; Function over clearX(), collections, hinted arm
            slot          ; compiled arm: the field's slot index, else nil
            slot-default  ; compiled arm, fields without presence: the default
                          ; in slot representation, read back when the slot is nil
            enum-kw-by-number]) ; {long -> keyword}, enum kind only

(def ^:private invoker-param-class
  {:int Integer/TYPE :long Long/TYPE :float Float/TYPE :double Double/TYPE
   :boolean Boolean/TYPE :string String
   :bytes com.google.protobuf.ByteString})

(defn- collection-invokers
  "Invokers for a repeated or map field: the bulk setter (addAllX / putAllX),
  the bulk getter (getXList / getXMap) and clearX. The setter is only offered
  together with clear, because the bulk accessors append and merge where the
  reflection API's setField replaces; the codec clears first to keep that."
  [^Class builder-class ^Class msg-class ^String suffix
   ^String set-name ^Class set-param ^String get-name ^Class get-return]
  (let [clear (invoke/getter-invoker builder-class (str "clear" suffix) builder-class)]
    {:set   (when clear (invoke/setter-invoker builder-class set-name set-param))
     :get   (invoke/getter-invoker msg-class get-name get-return)
     :clear clear}))

(defn- field-invokers
  "Typed-accessor invokers for a field of a generated-class prototype;
  {:set :get :has :clear}-shaped, each independently nil when underivable.
  Only generated classes get any: compiled and DynamicMessage prototypes
  have no typed accessors.

  Singular scalars and messages: setX / getX / hasX. Enums: setXValue(int) /
  getXValue(), which protoc emits for open enums only — closed (proto2) enums
  find nothing and stay on the reflection path. Repeated: addAllX / getXList;
  maps: putAllX / getXMap. Repeated enums and enum-valued maps take generated
  Java enum classes through those accessors, so they keep the reflection path
  too."
  [^Message prototype ^Descriptors$FieldDescriptor fd kind ^Message nested-proto
   has-presence? repeated? map-field ^clj_protobuf.runtime.FieldHandle val-handle]
  (if-not (instance? GeneratedMessage prototype)
    {}
    (let [suffix        (invoke/accessor-suffix (.getName fd))
          builder-class (.getClass (.newBuilderForType prototype))
          msg-class     (.getClass prototype)]
      (cond
        map-field
        (if (= :enum (.-kind val-handle))
          {}
          (collection-invokers builder-class msg-class suffix
                               (str "putAll" suffix) java.util.Map
                               (str "get" suffix "Map") java.util.Map))

        repeated?
        (if (= kind :enum)
          {}
          (collection-invokers builder-class msg-class suffix
                               (str "addAll" suffix) Iterable
                               (str "get" suffix "List") java.util.List))

        (= kind :enum)
        {:set (invoke/setter-invoker builder-class (str "set" suffix "Value") Integer/TYPE)
         :get (invoke/getter-invoker msg-class (str "get" suffix "Value") Integer/TYPE)
         :has (when has-presence?
                (invoke/getter-invoker msg-class (str "has" suffix) Boolean/TYPE))}

        :else
        (let [value-class (if (= kind :message)
                            (.getClass nested-proto)
                            (invoker-param-class kind))]
          {:set (invoke/setter-invoker builder-class (str "set" suffix) value-class)
           :get (invoke/getter-invoker msg-class (str "get" suffix) value-class)
           :has (when has-presence?
                  (invoke/getter-invoker msg-class (str "has" suffix) Boolean/TYPE))})))))

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
        compiled  (message/compiled-message? prototype)
        ;; The nested prototype has the parent's concrete class: a generated
        ;; parent yields the generated nested class through its builder, and
        ;; a compiled parent yields the compiled nested type straight from
        ;; the compiler — including for a map's entry type, so the entry's
        ;; key and value handles, and through them a message-valued map's
        ;; children, are compiled too. DynamicMessage yields DynamicMessage.
        nested    (when (= kind :message)
                    (if compiled
                      (message/prototype (.getMessageType fd))
                      (-> (.newBuilderForType prototype)
                          (.newBuilderForField fd)
                          (.getDefaultInstanceForType))))
        [kh vh]   (when map-field
                    (let [ed (.getMessageType fd)]
                      [(make-handle nested (.findFieldByName ed "key"))
                       (make-handle nested (.findFieldByName ed "value"))]))
        children  (when (and (= kind :message) (not map-field))
                    ;; Delayed: descriptors can be cyclic (a message containing
                    ;; itself), and an eager walk would never terminate. An
                    ;; array, because the codec walks it by index on every
                    ;; nested message; each child carries its own keys.
                    (delay
                      (object-array
                       (mapv (fn [^Descriptors$FieldDescriptor cfd]
                               (make-handle nested cfd))
                             (.getFields (.getDescriptorForType nested))))))
        enum-type (when (= kind :enum) (.getEnumType fd))]
    (let [invokers (field-invokers prototype fd kind nested (.hasPresence fd)
                                   repeated map-field vh)]
      (->FieldHandle fd kind repeated map-field (.hasPresence fd)
                   enum-type
                   kh vh nested children
                   (naming/field-key (.getName fd))
                   (keyword (.getName fd))
                   ;; Interning a keyword per read is measurable on enum-heavy
                   ;; messages, and EnumDescriptor.findValueByName is a string
                   ;; concatenation plus a pool lookup per write. The value set
                   ;; is small and known now, so all three directions are tables.
                   (when enum-type
                     (into {}
                           (map (fn [^Descriptors$EnumValueDescriptor v]
                                  [v (keyword (.getName v))]))
                           (.getValues enum-type)))
                   (when enum-type
                     (into {}
                           (map (fn [^Descriptors$EnumValueDescriptor v]
                                  [(keyword (.getName v)) v]))
                           (.getValues enum-type)))
                   (when enum-type
                     (into {}
                           (map (fn [^Descriptors$EnumValueDescriptor v]
                                  [(long (.getNumber v)) v]))
                           (.getValues enum-type)))
                   (:set invokers) (:get invokers) (:has invokers)
                   (:clear invokers)
                   (when compiled (long (.getIndex fd)))
                   (when (and compiled (not repeated) (not map-field)
                              (not= kind :message) (not (.hasPresence fd)))
                     (compile/slot-default fd))
                   (when enum-type
                     (into {}
                           (map (fn [^Descriptors$EnumValueDescriptor v]
                                  [(long (.getNumber v)) (keyword (.getName v))]))
                           (.getValues enum-type)))))))

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
