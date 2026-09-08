(ns clj-protobuf.impl.invoke
  "Typed-accessor invokers, built once per field with LambdaMetafactory.

  protobuf-java's reflection API pays a FieldAccessorTable lookup on every
  setField/getField. When the prototype is a generated class, the typed
  accessors (setFooBar, getFooBar, hasFooBar) are right there — and a
  metafactory-generated BiFunction/Function calling one runs at direct-interop
  speed (measured ~3.5 ns/op, vs ~µs through reflection paths), including the
  primitive boxing bridge the instantiated method type declares.

  Everything here is best-effort by construction, in the same spirit as the
  Java-class hint: derive protoc's accessor name, let findVirtual verify it
  exists with the expected signature, and return nil on ANY failure —
  including LambdaMetafactory itself being unavailable, which is what happens
  under native-image, where the codec silently keeps its reflection path.
  A wrong derivation is never wrong bytes, only a missed optimisation."
  (:import [java.lang.invoke CallSite LambdaMetafactory MethodHandles MethodType]
           [java.util.function BiFunction Function]))

(set! *warn-on-reflection* true)

(def ^:private forbidden-suffixes
  "Accessor suffixes protoc refuses to generate bare, because they would
  collide with a method on java.lang.Object or on the Message interfaces;
  protoc appends `_` to each, so a field named `class` is read with
  getClass_(). From compiler/java/names.cc, whose IsForbidden compares
  exactly what UnderscoresToCamelCase produced.

  Precision IS load-bearing here, contrary to the note below, for the one
  case where the wrong name resolves instead of failing: a field named
  serialized_size derives getSerializedSize, which exists on every message
  and returns an int, so findVirtual matches and the invoker returns the
  message's serialized size in place of the field. Same shape for the rest
  of this set. Deriving the same names protoc generates is the fix; the
  suffix is shared with the emitter, which needs it for the same reason."
  #{"Class" "DefaultInstanceForType" "ParserForType" "SerializedSize"
    "AllFields" "DescriptorForType" "InitializationErrorString" "UnknownFields"
    "CachedSize"})

(defn accessor-suffix
  "protoc's UnderscoresToCamelCase for accessor names: drop underscores,
  capitalise the letter after an underscore or digit, preserve existing case
  elsewhere. repeat_count -> RepeatCount, f10 -> F10, camelCaseField ->
  CamelCaseField, with protoc's trailing underscore on the forbidden set
  above. A miss elsewhere just fails findVirtual and the field stays on the
  reflection path."
  ^String [^String s]
  (let [sb (StringBuilder. (.length s))]
    (loop [i 0 cap? true]
      (if (= i (.length s))
        (let [out (.toString sb)]
          (if (contains? forbidden-suffixes out) (str out "_") out))
        (let [c (.charAt s i)]
          (cond
            (= c \_)
            (recur (inc i) true)

            (Character/isDigit c)
            (do (.append sb c) (recur (inc i) true))

            :else
            (do (.append sb (if cap? (Character/toUpperCase c) c))
                (recur (inc i) false))))))))

(def ^:private boxed
  {Integer/TYPE Integer
   Long/TYPE    Long
   Float/TYPE   Float
   Double/TYPE  Double
   Boolean/TYPE Boolean})

(defn- box ^Class [^Class c] (get boxed c c))

(defn- make-lambda
  "Drive LambdaMetafactory and extract the functional instance.
  MethodHandle.invoke is signature-polymorphic and unreachable from Clojure;
  invokeWithArguments is an ordinary method and the call happens once per
  field, not per op."
  [^java.lang.invoke.MethodHandles$Lookup lookup target
   ^Class iface ^String iface-method
   ^MethodType iface-type ^MethodType instantiated]
  (let [site ^CallSite (LambdaMetafactory/metafactory
                        lookup iface-method
                        (MethodType/methodType iface)
                        iface-type target instantiated)]
    (.invokeWithArguments (.getTarget site) (java.util.ArrayList.))))

(defn setter-invoker
  "(BiFunction builder value) applying builderClass.<name>(paramClass), or nil."
  ^BiFunction [^Class builder-class ^String method-name ^Class param-class]
  (try
    (let [lookup (MethodHandles/lookup)
          target (.findVirtual lookup builder-class method-name
                               (MethodType/methodType builder-class
                                                      ^"[Ljava.lang.Class;" (into-array Class [param-class])))]
      ^BiFunction
      (make-lambda lookup target BiFunction "apply"
                   (MethodType/methodType Object ^"[Ljava.lang.Class;" (into-array Class [Object Object]))
                   (MethodType/methodType builder-class
                                          ^"[Ljava.lang.Class;" (into-array Class [builder-class (box param-class)]))))
    (catch Throwable _ nil)))

(defn getter-invoker
  "(Function msg) applying msgClass.<name>() returning returnClass boxed, or nil."
  ^Function [^Class msg-class ^String method-name ^Class return-class]
  (try
    (let [lookup (MethodHandles/lookup)
          target (.findVirtual lookup msg-class method-name
                               (MethodType/methodType return-class))]
      ^Function
      (make-lambda lookup target Function "apply"
                   (MethodType/methodType Object ^"[Ljava.lang.Class;" (into-array Class [Object]))
                   (MethodType/methodType (box return-class)
                                          ^"[Ljava.lang.Class;" (into-array Class [msg-class]))))
    (catch Throwable _ nil)))
