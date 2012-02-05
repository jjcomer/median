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
  (median-skeleton l #(rand-nth %)))

(declare deter-median)

(defn find-median-of-medians
  "Find the median of medians of list l using partitions of size p"
  [l p]
  (let [l (into [] l)]
    (if (< (count l) p)
     (nth (into [] (sort l)) (ceil (/ (count l) 2)))
     (let [subvecs (partition p
                              (subvec l 0 (- (count l) (mod (count l) p))))
           medians (map (fn [n] (nth (sort n) (floor (/ p 2)))) subvecs)]
       (deter-median (into [] medians) p)))))

(defn deter-median
  "A median finding algorithm with asymptotically linear running time"
  [l p]
  (median-skeleton l #(find-median-of-medians % p)))