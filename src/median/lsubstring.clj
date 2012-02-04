(ns median.lsubstring)

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
  (let [empty-chart (createEmptyChart (count s1) (count s2))]))

(defn longest-subsequence
  "Given two strings, the longest subsequence is returned"
  [s1 s2]
  (let [filled-chart (fill-chart s1 s2)]
    (loop [row (dec (count s1))
           col (dec (count s2))
           acc []]
      (if (or (= 0 row) (= 0 col))
        (apply str (reverse acc))
        (let [[nr nc] (:parent (get-in filled-chart [row col]))]
          (if (= -1 x y)
            (recur (+ row nr) (+ col nc) (conj acc (nth s1 row)))
            (recur (+ row nr) (+ col nc) acc)))))))
