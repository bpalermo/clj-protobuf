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

(defn accessor-suffix
  "protoc's UnderscoresToCamelCase for accessor names: drop underscores,
  capitalise the letter after an underscore or digit, preserve existing case
  elsewhere. repeat_count -> RepeatCount, f10 -> F10, camelCaseField ->
  CamelCaseField. Precision is not load-bearing — a miss just fails
  findVirtual and the field stays on the reflection path."
  ^String [^String s]
  (let [sb (StringBuilder. (.length s))]
    (loop [i 0 cap? true]
      (if (= i (.length s))
        (.toString sb)
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
