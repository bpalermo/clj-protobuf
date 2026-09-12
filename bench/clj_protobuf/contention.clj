(ns clj-protobuf.contention
  "Does the codec scale across threads, or is there something shared in it?

  The other benchmark measures one thread and answers 'how fast'. This one
  answers 'how many at once', which is a different question with a different
  failure mode: a shared lock costs almost nothing on one thread and caps the
  whole process on eight, so //bench:run cannot see it and a CPU profile
  cannot either — a thread parked on a monitor is not on-CPU.

  That is not hypothetical. Until 0.2.5 two process-wide
  Collections.synchronizedMaps sat on the compiled arm's per-message path
  (the required-field memo, reached from .build on every message built
  including nested ones, and the parser registry, reached on every decode).
  synchronizedMap locks reads too, so a cache HIT still serialized. The
  compiled arm's encode measured 0.99x from 1 to 8 threads — eight threads
  delivering what one did — and the full pipeline COLLAPSED to below its own
  two-thread number at eight. Both memos now live on the CompiledType as
  delays, and the same measurement reads 8.48x.

  The hinted arm is the control: it runs through protoc's generated classes
  and touches none of our caches, so if both arms degrade together the cause
  is the machine, and if only the compiled arm does the cause is ours.

  Read the SCALING column, not the ops/s: absolute throughput depends on the
  box and on how warm the JIT is, but an arm that stops scaling while the
  control keeps scaling is a finding at any absolute number.

      bazel run //bench:contention              ; 1, 2, 4, 8 threads
      bazel run //bench:contention -- 1 2 4 8 16"
  (:require [clj-protobuf.bench :as bench])
  (:import [java.util.concurrent CountDownLatch]))

;; A reflective call inside an arm does not just make it slow, it makes the
;; measurement wrong: reflection dominates the timing and hides contention.
;; This namespace measured .build as scaling fine, once, for that reason.
;; Nothing here constructs an arm by hand for the same reason it does not
;; restate the corpus: both come from //bench:run's tables below, so the two
;; probes cannot drift apart and an arm added there appears here for free.
(set! *warn-on-reflection* true)

;; repeated-messages: 21 messages built per encode and parsed per decode, so a
;; per-message lock is reached 21 times an op — the density that made the
;; 0.2.4 monitor visible at all. A flatter shape would have hidden it.
(def ^:private shape
  (first (filter (comp #{:repeated-messages} :shape) bench/shapes)))

(defn- arms []
  (let [enc (bench/encode-arms shape)
        dec (bench/decode-arms shape)]
    (for [arm [:compiled :hinted]
          [dir by-arm] [["encode" enc] ["decode" dec]]]
      [(str (name arm) " " dir) (get by-arm arm)])))

(defn- ops-per-sec
  "Every thread starts at the same instant and runs the same count, so the
  wall time is the slowest thread's — which is the point when something
  serializes."
  ^double [task ^long threads ^long iters]
  (let [start (CountDownLatch. 1)
        done (CountDownLatch. threads)]
    (dotimes [_ threads]
      (.start (Thread. ^Runnable (fn [] (.await start) (dotimes [_ iters] (task)) (.countDown done)))))
    (let [t0 (do (.countDown start) (System/nanoTime))]
      (.await done)
      (/ (double (* threads iters))
         (/ (double (- (System/nanoTime) t0)) 1e9)))))

(defn -main [& args]
  (let [counts (if (seq args) (mapv parse-long args) [1 2 4 8])
        iters 40000]
    (println "clj-protobuf thread-scaling probe — read the scaling column")
    (println "hinted is the control: it touches none of this library's caches.\n")
    (doseq [[label task] (arms)]
      ;; warm this arm fully before timing it, so the 1-thread baseline the
      ;; rest is divided by is not a cold one
      (dotimes [_ 50000] (task))
      (let [base (ops-per-sec task 1 iters)]
        (println (format "%-16s %2d thr %10.0f ops/s" label 1 base))
        (doseq [t (remove #{1} counts)]
          (let [r (ops-per-sec task (long t) iters)]
            (println (format "%-16s %2d thr %10.0f ops/s   %5.2fx  (ideal %2dx)"
                             label t r (/ r base) t))))
        (println)))))
