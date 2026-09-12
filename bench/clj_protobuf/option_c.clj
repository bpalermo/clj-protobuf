(ns clj-protobuf.option-c
  "EXPERIMENT, not a feature: parse straight into Clojure values, skipping the
  compiled Message entirely.

  The typed read path being specced for the compiled arm makes the SECOND pass
  cheaper — decode to a CompiledMessage, then read its slots by index with the
  conversion emitted per field. Option C asks whether the second pass should
  exist at all: one tag loop that constructs the Clojure value as it parses, no
  Object[] of slots and no message in between.

  What it costs is the reason it is an experiment. A decoded value would no
  longer be a protobuf Message, so it cannot be re-serialised, handed to a grpc
  marshaller, or compared with one; and unknown fields — protobuf's
  forward-compatibility guarantee, the thing that lets a v1 service forward a
  v2 message intact — are dropped on the floor rather than preserved.

  This namespace hand-writes for ONE shape what a plugin would emit for every
  message, so the ceiling can be measured instead of argued about. It is
  deliberately not wired into anything.

      bazel run //bench:option_c

  DELETE THIS once the number is recorded in the plan. Its whole value is one
  measurement; kept around it becomes a second parser nobody holds to the
  equivalence suite."
  (:require [clj-protobuf.core :as pb]
            [clj-protobuf.bench :as bench]
            [fixtures.bench.shapes :as shapes])
  (:import [com.google.protobuf CodedInputStream Message]
           [java.util ArrayList]))

(set! *warn-on-reflection* true)

;; Tags, computed the way the emitter would: (field << 3) | wire-type.
;; Item:    sku 1/LEN=10   qty 2/VARINT=16   price 3/FIXED64=25
;; Payload: id 1/LEN=10  title 2/LEN=18  body 3/LEN=26
;;          created_at 4/VARINT=32  score 5/FIXED64=41  items 6/LEN=50
;; Reply:   name 1/LEN=10  payload 2/LEN=18

(defn- read-item
  "One Item as the plain map a nested field reads back as. Absent fields are
  omitted, which is what the codec's message->map does."
  [^CodedInputStream in]
  (let [len (.readRawVarint32 in)
        limit (.pushLimit in len)]
    (loop [sku nil qty nil price nil]
      (let [tag (.readTag in)]
        (case tag
          0  (do (.popLimit in limit)
                 (cond-> {}
                   sku (assoc :sku sku)
                   qty (assoc :qty qty)
                   price (assoc :price price)))
          10 (recur (.readString in) qty price)
          16 (recur sku (.readInt32 in) price)
          25 (recur sku qty (.readDouble in))
          (do (.skipField in tag) (recur sku qty price)))))))

(defn- read-payload [^CodedInputStream in]
  (let [len (.readRawVarint32 in)
        limit (.pushLimit in len)]
    (loop [id nil title nil body nil created-at nil score nil ^ArrayList items nil]
      (let [tag (.readTag in)]
        (case tag
          0  (do (.popLimit in limit)
                 (cond-> {}
                   id (assoc :id id)
                   title (assoc :title title)
                   body (assoc :body body)
                   created-at (assoc :created-at created-at)
                   score (assoc :score score)
                   (and items (pos? (.size items))) (assoc :items (vec items))))
          10 (recur (.readString in) title body created-at score items)
          18 (recur id (.readString in) body created-at score items)
          26 (recur id title (.readString in) created-at score items)
          32 (recur id title body (.readInt64 in) score items)
          41 (recur id title body created-at (.readDouble in) items)
          50 (let [^ArrayList l (or items (ArrayList.))]
               (.add l (read-item in))
               (recur id title body created-at score l))
          (do (.skipField in tag) (recur id title body created-at score items)))))))

(defn parse-reply
  "bytes -> a Reply record, in one pass."
  [^bytes bs]
  (let [in (CodedInputStream/newInstance bs)]
    (loop [nm nil payload nil]
      (let [tag (.readTag in)]
        (case tag
          0  (shapes/->Reply nm payload)
          10 (recur (.readString in) payload)
          18 (recur nm (read-payload in))
          (do (.skipField in tag) (recur nm payload)))))))

;; ---------------------------------------------------------------------------

(defn -main [& _]
  (println "option C — parse straight into Clojure values, no CompiledMessage")
  (println "standard = pb/decode to a compiled message, then proto->Reply\n")
  (doseq [shape [:realistic :dense]]
    (let [v (bench/values shape)
          bs (pb/encode ^Message (shapes/Reply->proto v))
          standard #(shapes/proto->Reply (pb/decode shapes/Reply-prototype bs))
          direct #(parse-reply bs)]
      ;; a number from a path that returns something different is not a number
      (assert (= (standard) (direct))
              (str shape ": option C did not reproduce the standard path's value"))
      (dotimes [_ 50000] (standard) (direct))
      (let [t (fn [f] (let [t0 (System/nanoTime)] (dotimes [_ 100000] (f))
                        (/ (double (- (System/nanoTime) t0)) 100000)))
            a (t standard) b (t direct)]
        (println (format "%-11s %d B  standard %8.0f ns   option C %8.0f ns   %+.1f%%"
                         (name shape) (alength bs) a b (* 100.0 (/ (- b a) a))))))))
