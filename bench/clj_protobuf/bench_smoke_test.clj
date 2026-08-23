(ns clj-protobuf.bench-smoke-test
  "Every benchmark arm runs once and the protobuf arms agree on the bytes —
  so the bench code cannot rot while staying out of CI's hot path."
  (:require [clj-protobuf.bench :as bench]
            [clojure.test :refer [deftest is testing]]))

(deftest every-encode-arm-runs
  (doseq [shape bench/shapes]
    (testing (str (:shape shape))
      (let [arms (#'bench/encode-arms shape)]
        (doseq [[arm f] arms]
          (is (some? (f)) (str arm " produced output")))))))

(deftest every-decode-arm-runs
  (doseq [shape bench/shapes]
    (testing (str (:shape shape))
      (let [arms (#'bench/decode-arms shape)]
        (doseq [[arm f] arms]
          (is (some? (f)) (str arm " produced output")))))))

(deftest protobuf-arms-agree-on-bytes
  (doseq [{:keys [shape to] :as s} bench/shapes]
    (testing (str shape)
      (let [arms  (#'bench/encode-arms s)
            base  ^bytes ((:hinted arms))
            other (keep arms [:dynamic :java])]
        (doseq [f other]
          (is (java.util.Arrays/equals base ^bytes (f))
              (str shape " arms byte-identical")))))))
