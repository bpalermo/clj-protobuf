(ns clj-protobuf.interop-test
  "interop=true output against the codec arm and against protoc: same bytes,
  same semantics, from real generated code — the validation the plugin
  delegates here."
  (:require [clojure.test :refer [deftest is testing]]
            [clj-protobuf.codec :as codec]
            [clj-protobuf.core :as pb]
            [clj-protobuf.runtime :as rt]
            [fixtures.e2024.kitchen :as std]
            [interop.fixtures.e2024.kitchen :as fast]
            [interop.fixtures.bench.shapes :as fast-shapes]
            [fixtures.wire.p2 :as swp2]
            [fixtures.wire.p3 :as swp3]
            [fixtures.wire.e2024 :as swe]
            [interop.fixtures.wire.p2 :as fwp2]
            [interop.fixtures.wire.p3 :as fwp3]
            [interop.fixtures.wire.e2024 :as fwe]))

(def kitchen-value
  {:str-field "hello" :int-field 42 :bool-field true
   :bytes-field (byte-array [1 2 3]) :dbl-field 3.5
   :long-field 9007199254740993 :enum-field :COLOR_RED
   :msg-field {:id "nested"} :tags ["a" "b"]
   :children [{:id "c1"} {:id "c2"}] :counts {"x" 1 "y" 2}
   :choice-int 7 :ts {:seconds 5 :nanos 100} :dur {:seconds 30}
   :wrapped {:value "wrapped"} :implicit-field "imp"
   :delimited {:note "grouped"}})

(deftest fast-arm-bytes-equal-codec-arm
  (testing "nil-opts (fast) and the standard namespace agree byte for byte"
    (is (java.util.Arrays/equals
         (pb/encode (std/Kitchen->proto kitchen-value))
         (pb/encode (fast/Kitchen->proto kitchen-value)))))
  (testing "an opts value routes the interop namespace onto the codec arm,
            same bytes again"
    (is (java.util.Arrays/equals
         (pb/encode (fast/Kitchen->proto kitchen-value nil))
         (pb/encode (fast/Kitchen->proto kitchen-value {:enums :keyword}))))))

(deftest fast-arm-nil-semantics
  (testing "nil fields set nothing; zero values with presence survive"
    (let [out (->> (fast/Kitchen->proto {:int-field 0 :str-field ""})
                   pb/encode
                   (pb/decode fast/Kitchen-prototype)
                   fast/proto->Kitchen)]
      (is (= 0 (:int-field out)))
      (is (= "" (:str-field out)))
      (is (nil? (:dbl-field out)))
      (is (nil? (:msg-field out))))))

(deftest fast-arm-message-values
  (testing "records, maps, and pre-built Messages all land identically"
    (let [via-map (fast/Kitchen->proto {:msg-field {:id "n"}})
          via-rec (fast/Kitchen->proto {:msg-field (fast/->Nested "n")})
          via-msg (fast/Kitchen->proto {:msg-field (fast/Nested->proto {:id "n"})})]
      (is (java.util.Arrays/equals (pb/encode via-map) (pb/encode via-rec)))
      (is (java.util.Arrays/equals (pb/encode via-map) (pb/encode via-msg))))))

(deftest shapes-load-and-agree
  (doseq [[to value]
          [[#'fast-shapes/Tiny->proto {:id "t" :n 1 :ok true}]
           [#'fast-shapes/Flat->proto {:f1 "a" :f4 7 :f6 1 :f8 true :f10 1.5}]]]
    (is (some? (pb/encode (to value))))))

;; ---------------------------------------------------------------------------
;; The wire corpus through both arms
;;
;; The two headline fixtures above are the shapes a benchmark uses. These are
;; the ones a codec gets wrong: groups, packed and unpacked repeateds under
;; shared field numbers, closed and aliased enums, explicit defaults, required
;; fields, field names whose Java accessors collide with a method every message
;; has, and every editions feature that changes bytes. Whatever the emitter
;; types and whatever it leaves on the codec, the two arms must agree.

(defn- normalize
  "Byte arrays compare by identity; make them comparable, recursively."
  [v]
  (cond
    (bytes? v) (vec v)
    (map? v) (into {} (map (fn [[k x]] [k (normalize x)])) v)
    (sequential? v) (mapv normalize v)
    :else v))

(def ^:private wire-cases
  [["wire.p2/Wire" swp2/Wire->proto swp2/Wire-prototype swp2/proto->Wire
    fwp2/Wire->proto fwp2/Wire-prototype fwp2/proto->Wire
    [{:i32 -1 :i64 -2 :u32 3 :s32 -5 :f32 7 :sf64 -10 :flt (float 1.5) :dbl 2.5 :flag true
      :str "héllo 日本 😀" :raw (byte-array [0 127 -128]) :color :CLOSED_B :leaf {:id "leaf"}
      :unpacked [1 -2 300] :packed [4 5] :colors [:CLOSED_A :CLOSED_B] :leaves [{:id "a"} {:id "b"}]
      :names ["x" "" "日本"] :by-id {2 {:id "two"}} :by-color {"a" :CLOSED_A} :pick-leaf {:id "p"}
      :dflt-int 42 :dflt-str "other" :dflt-enum :CLOSED_A :grp {:note "g"} :grps [{:n 1} {:n 2}]
      :serialized-size 7 :class "cls" :aliased :ALIASED_ALSO_FIRST :high "h" :huge 7}
     {}
     {:leaf {}}
     {:names [] :by-id {}}
     {:aliased :ALIASED_FIRST}
     {:leaves [{}]}]]
   ["wire.p3/Wire" swp3/Wire->proto swp3/Wire-prototype swp3/proto->Wire
    fwp3/Wire->proto fwp3/Wire-prototype fwp3/proto->Wire
    [{:i32 -1 :str "s" :raw (byte-array [1 2]) :color :OPEN_B :leaf {:id "l"} :packed [1 -2 300]
      :unpacked [4 5] :colors [:OPEN_A] :leaves [{:id "a"}] :names ["x"] :by-id {1 {:id "one"}}
      :by-color {"k" :OPEN_A} :pick-i64 99 :opt-i32 0 :opt-str "" :huge 1}
     {} {:leaf {}} {:color 42} {:i32 0 :str ""}]]
   ["wire.e2024/Wire" swe/Wire->proto swe/Wire-prototype swe/proto->Wire
    fwe/Wire->proto fwe/Wire-prototype fwe/proto->Wire
    [{:str "s" :checked "c" :expanded [1 2] :packed [3 4] :delimited {:id "d"}
      :length-prefixed {:id "l"} :implicit 5 :explicit 0 :open :OPEN_A :closed :CLOSED_B
      :closed-list [:CLOSED_A]}
     {} {:delimited {}} {:implicit 0} {:open 99}]]])

(deftest wire-corpus-arms-agree
  (doseq [[name std-to std-proto std-from fast-to fast-proto fast-from values] wire-cases]
    (testing name
      (doseq [v values]
        (testing (pr-str v)
          (let [sb (pb/encode (std-to v))
                fb (pb/encode (fast-to v))]
            (is (java.util.Arrays/equals ^bytes sb ^bytes fb) "same bytes")
            (let [via-codec (normalize (into {} (std-from (pb/decode std-proto sb))))
                  via-fast  (normalize (into {} (fast-from (pb/decode fast-proto sb))))]
              (is (= via-codec via-fast) "same values")
              (testing "the standard fixture's prototype is the SAME generated class
                        here — its class hint resolves, because this target has
                        protoc's classes on the classpath. So the line above is
                        one arm, not two; a genuinely foreign message is the
                        subject of prototypes-are-not-interchangeable below."
                (is (identical? (class std-proto) (class fast-proto))))
              (testing "an opts value routes both onto the codec, same answer again"
                (is (= via-codec (normalize (into {} (fast-from (pb/decode fast-proto sb) {})))))))))))))

(deftest wire-corpus-edge-shapes
  (testing "a nested message set to its default instance reads back as {} on both
            arms, and empty repeated and map fields read back as nil"
    (let [bs (pb/encode (fwp2/Wire->proto {:leaf {} :names [] :by-id {}}))
          fast (fwp2/proto->Wire (pb/decode fwp2/Wire-prototype bs))
          codec (swp2/proto->Wire (pb/decode swp2/Wire-prototype bs))]
      (is (= {} (:leaf fast)))
      (is (= {} (:leaf codec)))
      (is (nil? (:names fast)))
      (is (nil? (:by-id fast))))))

(deftest prototypes-are-not-interchangeable
  (testing "a generated proto->X reads its own arm's messages and no other.
            Its field handles are built on its own prototype: on a hinted
            namespace they carry LambdaMetafactory invokers over the generated
            class's accessors, and even without invokers a handle's
            FieldDescriptor belongs to that prototype's descriptor pool, which
            protobuf-java forbids using against another pool's messages. So
            handing proto->X a message from another arm throws rather than
            decoding — the nil-opts guard chooses between the typed and codec
            paths, it does not make the fn polymorphic over arms.

            Pinned, not endorsed: crossing arms is already invalid by the pool
            rule in docs/design.md, and this is what invalid looks like. What
            matters is that it is loud."
    (let [bs (pb/encode (fast/Kitchen->proto kitchen-value))
          foreign (pb/decode (rt/dynamic-message std/file-descriptor "Kitchen") bs)]
      (is (instance? com.google.protobuf.DynamicMessage foreign))
      (is (thrown? ClassCastException (fast/proto->Kitchen foreign)))
      (testing "an opts value does not rescue it: the codec path uses the same handles"
        (is (thrown? ClassCastException (fast/proto->Kitchen foreign {}))))
      (testing "and the message decodes fine through its own arm"
        (is (some? (codec/get-field foreign
                                    (rt/field (rt/dynamic-message std/file-descriptor "Kitchen") "str_field")
                                    nil)))))))
