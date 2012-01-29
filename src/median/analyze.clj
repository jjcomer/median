(ns median.analyze
  (:use [median core]
        [incanter core charts datasets stats]))

(defn test-lists
  "Generate test lists of powers of p"
  [n p f]
  (map #(rand-list (f p %)) (range n)))

(defmacro time-it
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn run-test
  [l s]
  (let [rm-data (map (fn [n] [(count l) :rm n])
                     (take s (repeatedly (fn [] (time-it (rand-median l))))))
        dm-data (map (fn [n] [(count l) :dm n])
                     (take s (repeatedly (fn [] (time-it
                                                 (deter-median l 5))))))]
    (concat rm-data dm-data)))

(defn build-dataset
  [n p s]
  (let [test-data (test-lists n p pow)]
    (dataset [:count :alg :time]
             (reduce concat (map #(run-test % s) test-data)))))

(defn generate-plot
  [n p s]
  (with-data (build-dataset n p s)
    (let [rm-data ($where {:alg {:eq :rm}})
          dm-data ($where {:alg {:eq :dm}})
          rm-lm (linear-model ($ :time rm-data) ($ :count rm-data))
          dm-lm (linear-model ($ :time dm-data) ($ :count dm-data))]
      (doto (scatter-plot ($ :count) ($ :time)
                     :group-by :alg
                     :x-label "Input Size"
                     :y-label "Time (ms)")
        (add-lines ($ :count rm-data) (:fitted rm-lm))
        (add-lines ($ :count dm-data) (:fitted dm-lm))))))

(defn run-deter-test
  [l s p]
  (let [partitions (map inc (range 2 p))
        data (map (fn [n]
                    (map (fn [t] [(count l) n t])
                         (take s
              (repeatedly (fn [] (time-it (deter-median l n))))))) partitions)]
    (reduce concat data)))

(defn build-deter-dataset
  [n p s part]
  (let [test-data (test-lists n p pow)]
    (dataset [:count :parts :time]
             (reduce concat (map #(run-deter-test % s part) test-data)))))

(defn generate-deter-plot
  [n p s part]
  (with-data (build-deter-dataset n p s part)
    (scatter-plot ($ :count) ($ :time)
                  :group-by :parts
                  :x-label "Input Size"
                  :y-label "Time (ms)")))