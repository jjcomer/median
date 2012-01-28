(ns median.core
  (:use [clojure.math.numeric-tower :only [floor]]))

(defn rand-list
  "Generates a random vector of length n with unique
   values inclusive 1 to exclusive n"
  [n]
  (shuffle (map inc (range n))))

(defn rand-median
  "A randomized median finding algorithm"
  [l]
  (loop [l l
         median (floor (/ (count l) 2))]
    (if (= 1 (count l))
      (first l)
      (let [pivot (nth l (rand-int (count l)))
            less-part (filter #(< % pivot) l)
            great-part (filter #(> % pivot) l)
            rank (inc (count less-part))]
        (cond
         (= rank median) pivot
         (< median rank) (recur less-part
                                median)
         :else (recur great-part
                      (- median rank)))))))
