(ns median.core
  (:use [clojure.math.numeric-tower :only [floor ceil]]))

(defn rand-list
  "Generates a random vector of length n with unique
   values inclusive 1 to exclusive n"
  [n]
  (shuffle (map inc (range n))))

(defn- median-skeleton
  "The skeleton for the median finding algorithms"
  [l pivot-fn]
  (loop [l l
         median (floor (/ (count l) 2))]
    (if (= 1 (count l))
      (first l)
      (let [pivot (pivot-fn l)
            less-part (filter #(< % pivot) l)
            great-part (filter #(> % pivot) l)
            rank (inc (count less-part))]
        (cond
         (= rank median) pivot
         (< median rank) (recur less-part
                                median)
         :else (recur great-part
                      (- median rank)))))))

(defn rand-median
  "A randomized median finding algorithm"
  [l]
  (median-skeleton l #(floor (/ (count %) 2))))

(declare deter-median)

(defn find-median-of-medians
  [l]
  (let [l (into [] l)]
    (if (< (count l) 5)
     (nth (into [] (sort l)) (ceil (/ (count l) 2)))
     (let [subvecs (partition 5
                              (subvec l 0 (- (count l) (mod (count l) 5))))
           medians (map (fn [n] (nth (sort n) 2)) subvecs)]
       (deter-median (into [] medians))))))

(defn deter-median
  "A median finding algorithm with asymptotically linear running time"
  [l]
  (median-skeleton l find-median-of-medians))