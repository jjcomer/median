(ns median.lsubsequence)

;The answer for these strings is MJAU
(def testString1 "XMJYAUZ")
(def testString2 "MZJAWXU")

(defn generateTestSting
  "Given a range r and a length l (max 26)
   a randomized test string is  generated"
  [r l]
  (let [glyphs (map #(char (+ 65 %)) (range (mod r 26)))]
    (apply str (take l (repeatedly #(rand-nth glyphs))))))

(defrecord Cell [value parent])

(defn createEmptyChart
  "Fills a chart of y rows and x cols with 0s"
  [y x]
  (vec (take y (repeatedly #(vec (repeat x (Cell. 0 [])))))))

(defn fill-chart
  [s1 s2]
  (loop [coords (for [row (range (count s1))
                      col (range (count s2))]
                  [(inc row) (inc col)])
         chart (createEmptyChart (inc (count s1)) (inc (count s2)))]
    (if (empty? coords)
      chart
      (let [[row col] (first coords)
            get-value #(:value (get-in chart [%1 %2]))]
        (if (= (nth s1 (dec row)) (nth s2 (dec col)))
          (recur (rest coords) (assoc-in chart [row col]
                                         (Cell.
                                          (inc (get-value (dec row) (dec col)))
                                          [-1 -1])))
          (recur (rest coords) (assoc-in chart [row col]
                                         (let [decr (get-value (dec row) col)
                                               decc (get-value row (dec col))]
                                           (if (> decr decc)
                                             (Cell. decr [-1 0])
                                             (Cell. decc [0 -1]))))))))))

(defn longest-subsequence
  "Given two strings, the longest subsequence is returned"
  [s1 s2]
  (let [filled-chart (fill-chart s1 s2)]
    (loop [row (count s1)
           col (count s2)
           acc []]
      (if (or (= 0 row) (= 0 col))
        (apply str (reverse acc))
        (let [[nr nc] (:parent (get-in filled-chart [row col]))]
          (if (= -1 nr nc)
            (recur (+ row nr) (+ col nc) (conj acc (nth s1 (dec row))))
            (recur (+ row nr) (+ col nc) acc)))))))
