(ns clj-protobuf.bench
  "The representation benchmark: protobuf through clj-protobuf (every arm)
  against protoc's own generated Java code and against JSON.

  Real payloads differ enormously in shape and the trade-offs do not behave the
  same across them — a flat scalar message is dominated by per-field overhead,
  a deeply nested one by allocation — so the corpus is a set of archetypes
  (see test/proto/fixtures/bench/shapes.proto), not one 'typical' message.

  Arms:
    :hinted   generated code + clj-protobuf, Java-class hints resolving
              (fixtures.bench.shapes with //test/proto:fixtures_java_proto on
              the classpath)
    :interop  the same shapes emitted with interop=true (plugin 0.6.0+,
              interop.fixtures.bench.shapes) — the same prototypes as :hinted
              driven through direct Java accessor calls instead of the codec,
              so the two columns differ only in emitted code.
    :compiled the same generated code with hints that cannot resolve
              (fixtures.bench-nohint.shapes) — the compiled codec, what any
              consumer without the generated classes gets. Run with
              -Dclj-protobuf.codec=dynamic (JAVA_TOOL_OPTIONS works under
              bazel run) and this column measures DynamicMessage instead.
    :java     protoc's generated Java builders driven directly — the floor
    :jsonista JSON via jackson (the fast JSON arm)
    :data-json JSON via org.clojure/data.json (the pure-Clojure JSON arm)

  Encode measures Clojure data -> bytes; decode measures bytes -> Clojure data
  (records for protobuf, keyword-keyed maps for JSON) — full pipelines, so the
  comparison is what an application actually pays.

  bazel run //bench:run          ; full criterium run, prints markdown tables
  bazel run //bench:run -- quick ; quick-bench (~10x faster, noisier)"
  (:require [clj-protobuf.core :as pb]
            [clojure.data.json :as data-json]
            [criterium.core :as crit]
            [fixtures.bench.shapes :as hinted]
            [fixtures.bench-nohint.shapes :as dynamic]
            [interop.fixtures.bench.shapes :as interop]
            [jsonista.core :as j])
  (:import [com.acme.fixtures.bench Flat RepeatedMessages Tiny]
           [com.google.protobuf Message]
           [java.lang.management ManagementFactory]))

;; A reflective call inside an arm is not a slow arm, it is a wrong number.
(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Corpus

(defn- item [i]
  {:sku (format "SKU-%05d" i) :qty (int (inc (mod i 7))) :price (+ 1.5 i)})

(defn- reply
  "The production shape: a reply wrapping a body-bearing payload and n line
  items, with `filler` characters of body. Varying the two knobs moves field
  count and byte count independently."
  [n filler]
  {:name "reply-1"
   :payload {:id "pl-0001"
             :title "Quarterly summary"
             :body (apply str (repeat filler \x))
             :created-at 1757462400000
             :score 0.9375
             :items (mapv item (range n))}})

(def values
  {:tiny {:id "t-1" :n 42 :ok true}
   :flat {:f1 "alpha" :f2 "beta" :f3 "gamma" :f4 7 :f5 1024 :f6 123456789012
          :f7 -42 :f8 true :f9 false :f10 3.5 :f11 2.25 :f12 "omega"}
   :deep {:id "d" :child {:id "l2" :child {:id "l3" :child {:id "l4" :leaf "leaf"}}}}
   :wide-repeated {:id "w" :items (mapv #(str "item-" %) (range 50))}
   :repeated-messages {:id "r" :rows (mapv (fn [i] {:id (str "row-" i) :n i :ok (even? i)})
                                           (range 20))}
   :map-heavy {:id "m" :counts (into {} (map (fn [i] [(str "k" i) i])) (range 50))}
   :enum-heavy {:id "e"
                :s1 :STATUS_ACTIVE :s2 :STATUS_PAUSED :s3 :STATUS_CLOSED :s4 :STATUS_ACTIVE
                :s5 :STATUS_PAUSED :s6 :STATUS_CLOSED :s7 :STATUS_ACTIVE :s8 :STATUS_PAUSED
                :history (vec (take 8 (cycle [:STATUS_ACTIVE :STATUS_PAUSED :STATUS_CLOSED])))}
   ;; The two production-shaped tiers: the SAME wire size carried two ways, so
   ;; that the per-field and per-byte terms separate instead of moving
   ;; together the way they do in every shape above. realistic is 30 leaf
   ;; values with its bulk in one filler string; dense is 120 across many
   ;; small line items. Byte targets and structure come from clj-grpc's soak
   ;; payload so the two corpora's tables need no conversion step.
   :realistic (reply 8 774)
   :dense (reply 38 60)})

(def shapes
  [{:shape :tiny              :to 'Tiny->proto              :proto 'Tiny-prototype              :from 'proto->Tiny}
   {:shape :flat              :to 'Flat->proto              :proto 'Flat-prototype              :from 'proto->Flat}
   {:shape :deep              :to 'Deep->proto              :proto 'Deep-prototype              :from 'proto->Deep}
   {:shape :wide-repeated     :to 'WideRepeated->proto      :proto 'WideRepeated-prototype      :from 'proto->WideRepeated}
   {:shape :repeated-messages :to 'RepeatedMessages->proto  :proto 'RepeatedMessages-prototype  :from 'proto->RepeatedMessages}
   {:shape :map-heavy         :to 'MapHeavy->proto          :proto 'MapHeavy-prototype          :from 'proto->MapHeavy}
   {:shape :enum-heavy        :to 'EnumHeavy->proto         :proto 'EnumHeavy-prototype         :from 'proto->EnumHeavy}
   {:shape :realistic         :to 'Reply->proto             :proto 'Reply-prototype             :from 'proto->Reply}
   {:shape :dense             :to 'Reply->proto             :proto 'Reply-prototype             :from 'proto->Reply}])

(defn- resolve-in [ns-sym sym] @(ns-resolve ns-sym sym))

(def arm-namespaces
  "The clj-protobuf arms and the fixture namespace each is emitted into. Same
  protos and the same generated symbols in all three — what differs is which
  prototype `rt/message` hands back, which is the whole subject of these
  tables. Every arm resolves the same `to`/`from`/`proto` symbols out of its
  own namespace, so an arm is added here and nowhere else."
  {:hinted   'fixtures.bench.shapes
   :interop  'interop.fixtures.bench.shapes
   :compiled 'fixtures.bench-nohint.shapes})

;; ---------------------------------------------------------------------------
;; The raw protobuf-java arm: protoc's builders driven directly, the floor the
;; other arms are measured against. Hand-written for the two headline shapes
;; plus the repeated one; the other shapes compare the remaining arms.

(defn- java-tiny ^Message [{:keys [id n ok]}]
  (-> (Tiny/newBuilder) (.setId id) (.setN (int n)) (.setOk (boolean ok)) (.build)))

(defn- java-flat ^Message [{:keys [f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12]}]
  (-> (Flat/newBuilder)
      (.setF1 f1) (.setF2 f2) (.setF3 f3)
      (.setF4 (int f4)) (.setF5 (int f5))
      (.setF6 (long f6)) (.setF7 (long f7))
      (.setF8 (boolean f8)) (.setF9 (boolean f9))
      (.setF10 (double f10)) (.setF11 (double f11))
      (.setF12 f12)
      (.build)))

(defn- java-repeated-messages ^Message [{:keys [id rows]}]
  (let [b (doto (RepeatedMessages/newBuilder) (.setId id))]
    (doseq [row rows] (.addRows b ^Tiny (java-tiny row)))
    (.build b)))

(def java-encoders
  {:tiny java-tiny
   :flat java-flat
   :repeated-messages java-repeated-messages})

(def java-parsers
  {:tiny #(Tiny/parseFrom ^bytes %)
   :flat #(Flat/parseFrom ^bytes %)
   :repeated-messages #(RepeatedMessages/parseFrom ^bytes %)})

;; ---------------------------------------------------------------------------
;; Measurement

(def ^:private quick? (atom false))

(defn- mean-ns
  "criterium's mean, in nanoseconds."
  [f]
  (let [result (if @quick? (crit/quick-benchmark* f {}) (crit/benchmark* f {}))]
    (* 1e9 (double (first (:mean result))))))

(defn- alloc-bytes
  "Allocated bytes per op, via the JVM's per-thread allocation counter,
  averaged over enough iterations to drown the sampling noise."
  [f]
  (let [tmx ^com.sun.management.ThreadMXBean (ManagementFactory/getThreadMXBean)
        tid (.threadId (Thread/currentThread))
        iters 20000]
    (dotimes [_ 5000] (f))                       ; warm: JIT + any lazy init
    (let [before (.getThreadAllocatedBytes tmx tid)]
      (dotimes [_ iters] (f))
      (let [after (.getThreadAllocatedBytes tmx tid)]
        (quot (- after before) iters)))))

(defn- measure [f]
  {:ns (mean-ns f) :bytes (alloc-bytes f)})

;; ---------------------------------------------------------------------------
;; Arms per shape

(def mapper (j/object-mapper {:decode-key-fn keyword}))

(defn encode-arms [{:keys [shape to]}]
  (let [value (values shape)]
    (cond-> (into {:jsonista  #(j/write-value-as-bytes value mapper)
                   :data-json #(data-json/write-str value)}
                  (map (fn [[arm ns-sym]]
                         (let [to-fn (resolve-in ns-sym to)]
                           [arm #(pb/encode ^Message (to-fn value))])))
                  arm-namespaces)
      (java-encoders shape)
      (assoc :java (let [enc (java-encoders shape)] #(pb/encode ^Message (enc value)))))))

(defn decode-arms [{:keys [shape to proto from]}]
  (let [value      (values shape)
        bytes      (pb/encode ^Message ((resolve-in (:hinted arm-namespaces) to) value))
        json-bytes (j/write-value-as-bytes value mapper)
        json-str   (data-json/write-str value)]
    (cond-> (into {:jsonista  #(j/read-value ^bytes json-bytes mapper)
                   :data-json #(data-json/read-str json-str :key-fn keyword)}
                  ;; each arm parses the bytes itself: the namespaces build
                  ;; independent descriptors, so a message from one arm is not
                  ;; the type another arm's prototype describes
                  (map (fn [[arm ns-sym]]
                         (let [proto-v (resolve-in ns-sym proto)
                               from-fn (resolve-in ns-sym from)]
                           [arm #(from-fn (pb/decode proto-v bytes))])))
                  arm-namespaces)
      (java-parsers shape)
      (assoc :java (let [parse (java-parsers shape)] #(parse bytes))))))

;; ---------------------------------------------------------------------------
;; Report

(def arm-order [:java :hinted :interop :compiled :jsonista :data-json])

(defn- fmt-ns [ns] (cond (nil? ns) "—"
                         (< ns 1000) (format "%.0f ns" ns)
                         :else (format "%.2f µs" (/ ns 1000.0))))

(defn- table [title results]
  (println (str "\n### " title "\n"))
  (println (str "| shape | " (clojure.string/join " | " (map name arm-order)) " |"))
  (println (str "|---|" (apply str (repeat (count arm-order) "---|"))))
  (doseq [{:keys [shape arms]} results]
    (println (str "| " (name shape) " | "
                  (clojure.string/join
                   " | "
                   (for [arm arm-order]
                     (if-let [{:keys [ns bytes]} (get arms arm)]
                       (str (fmt-ns ns) " / " bytes " B")
                       "—")))
                  " |"))))

(defn- run-op [op-name arms-fn]
  (println (str "\nmeasuring " op-name "..."))
  (vec (for [{:keys [shape] :as s} shapes]
         (do (println " " (name shape))
             {:shape shape
              :arms (into {} (for [[arm f] (arms-fn s)]
                               [arm (measure f)]))}))))

(defn -main [& args]
  (when (some #{"quick"} args) (reset! quick? true))
  (println "clj-protobuf representation benchmark")
  (println "arms: java = protoc's generated builders; hinted/interop/compiled = clj-protobuf;")
  (println "      jsonista/data.json = the same value as JSON. mean latency / allocated bytes per op.")
  (let [encode (run-op "encode" encode-arms)
        decode (run-op "decode" decode-arms)]
    (table "Encode (Clojure data -> bytes)" encode)
    (table "Decode (bytes -> Clojure data)" decode)
    (println "\nEDN:" (pr-str {:encode encode :decode decode}))))
