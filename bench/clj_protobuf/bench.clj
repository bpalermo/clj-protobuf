(ns clj-protobuf.bench
  "The representation benchmark: protobuf through clj-protobuf (both arms)
  against protoc's own generated Java code and against JSON.

  Real payloads differ enormously in shape and the trade-offs do not behave the
  same across them — a flat scalar message is dominated by per-field overhead,
  a deeply nested one by allocation — so the corpus is a set of archetypes
  (see test/proto/fixtures/bench/shapes.proto), not one 'typical' message.

  Arms:
    :hinted   generated code + clj-protobuf, Java-class hints resolving
              (fixtures.bench.shapes with //test/proto:fixtures_java_proto on
              the classpath)
    :dynamic  the same generated code with hints that cannot resolve
              (fixtures.bench-nohint.shapes) — the pure DynamicMessage arm
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
            [jsonista.core :as j])
  (:import [com.acme.fixtures.bench Flat RepeatedMessages Tiny]
           [com.google.protobuf Message]
           [java.lang.management ManagementFactory]))

;; ---------------------------------------------------------------------------
;; Corpus

(def values
  {:tiny {:id "t-1" :n 42 :ok true}
   :flat {:f1 "alpha" :f2 "beta" :f3 "gamma" :f4 7 :f5 1024 :f6 123456789012
          :f7 -42 :f8 true :f9 false :f10 3.5 :f11 2.25 :f12 "omega"}
   :deep {:id "d" :child {:id "l2" :child {:id "l3" :child {:id "l4" :leaf "leaf"}}}}
   :wide-repeated {:id "w" :items (mapv #(str "item-" %) (range 50))}
   :repeated-messages {:id "r" :rows (mapv (fn [i] {:id (str "row-" i) :n i :ok (even? i)})
                                           (range 20))}
   :map-heavy {:id "m" :counts (into {} (map (fn [i] [(str "k" i) i])) (range 50))}})

(def shapes
  [{:shape :tiny              :to 'Tiny->proto              :proto 'Tiny-prototype              :from 'proto->Tiny}
   {:shape :flat              :to 'Flat->proto              :proto 'Flat-prototype              :from 'proto->Flat}
   {:shape :deep              :to 'Deep->proto              :proto 'Deep-prototype              :from 'proto->Deep}
   {:shape :wide-repeated     :to 'WideRepeated->proto      :proto 'WideRepeated-prototype      :from 'proto->WideRepeated}
   {:shape :repeated-messages :to 'RepeatedMessages->proto  :proto 'RepeatedMessages-prototype  :from 'proto->RepeatedMessages}
   {:shape :map-heavy         :to 'MapHeavy->proto          :proto 'MapHeavy-prototype          :from 'proto->MapHeavy}])

(defn- resolve-in [ns-sym sym] @(ns-resolve ns-sym sym))

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

(defn- encode-arms [{:keys [shape to]}]
  (let [value      (values shape)
        hinted-to  (resolve-in 'fixtures.bench.shapes to)
        dynamic-to (resolve-in 'fixtures.bench-nohint.shapes to)]
    (cond-> {:hinted    #(pb/encode ^Message (hinted-to value))
             :dynamic   #(pb/encode ^Message (dynamic-to value))
             :jsonista  #(j/write-value-as-bytes value mapper)
             :data-json #(data-json/write-str value)}
      (java-encoders shape)
      (assoc :java (let [enc (java-encoders shape)] #(pb/encode ^Message (enc value)))))))

(defn- decode-arms [{:keys [shape to proto from]}]
  (let [value        (values shape)
        bytes        (pb/encode ^Message ((resolve-in 'fixtures.bench.shapes to) value))
        json-bytes   (j/write-value-as-bytes value mapper)
        json-str     (data-json/write-str value)
        hinted-proto  (resolve-in 'fixtures.bench.shapes proto)
        hinted-from   (resolve-in 'fixtures.bench.shapes from)
        dynamic-proto (resolve-in 'fixtures.bench-nohint.shapes proto)
        dynamic-from  (resolve-in 'fixtures.bench-nohint.shapes from)]
    (cond-> {:hinted    #(hinted-from (pb/decode hinted-proto bytes))
             :dynamic   #(dynamic-from (pb/decode dynamic-proto bytes))
             :jsonista  #(j/read-value ^bytes json-bytes mapper)
             :data-json #(data-json/read-str json-str :key-fn keyword)}
      (java-parsers shape)
      (assoc :java (let [parse (java-parsers shape)] #(parse bytes))))))

;; ---------------------------------------------------------------------------
;; Report

(def arm-order [:java :hinted :dynamic :jsonista :data-json])

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
  (println "arms: java = protoc's generated builders; hinted/dynamic = clj-protobuf;")
  (println "      jsonista/data.json = the same value as JSON. mean latency / allocated bytes per op.")
  (let [encode (run-op "encode" encode-arms)
        decode (run-op "decode" decode-arms)]
    (table "Encode (Clojure data -> bytes)" encode)
    (table "Decode (bytes -> Clojure data)" decode)
    (println "\nEDN:" (pr-str {:encode encode :decode decode}))))
