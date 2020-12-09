(ns day9
  (:require [util]))

(defn sums
  "Given a coll of numbers, returns all possible sums of two of those numbers."
  [nums]
  (for [[i x] (map-indexed vector nums)
        [j y] (map-indexed vector nums)
        :when (< i j)]
    (+ x y)))

(defn part-1 []
  (let [input (map #(Long/parseLong %) (util/input-lines "day9-input.txt"))]
    (first (for [ns (partition 26 1 input)
                 :let [possible (set (sums (butlast ns)))
                       n (last ns)]
                 :when (not (contains? possible n))]
             n))))

(defn part-2 []
  (let [input (mapv #(Long/parseLong %) (util/input-lines "day9-input.txt"))
        invalid-number (part-1)]
    (for [i (range (count input))
          j (range (count input))
          :when (< i j)
          :let [v (subvec input i (inc j))]
          :when (= invalid-number (reduce + 0 v))]
      (+ (apply min v)
         (apply max v)))))
