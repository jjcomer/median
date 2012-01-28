(ns median.analyze
  (:use [median core]
        [incanter core charts datasets]))

(defn test-lists
  "Generate test lists of powers of 10"
  [n p]
  (map #(rand-list (pow p %)) (range n)))

(defmacro time-it
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn run-test
  [l]
  (let [rm-data (take 3 (repeatedly #(time-it (rand-median l))))
        dm-data (take 3 (repeatedly #(time-it (deter-median l))))]
    [(count l) (/ (sum rm-data) 3.0) (/ (sum dm-data) 3.0)]))

(defn build-dataset
  [n p]
  (let [test-data (test-lists n p)]
    (dataset ["n" "Random" "Determinant"] (map run-test test-data))))