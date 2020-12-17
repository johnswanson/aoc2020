(ns day17
  (:require [util]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn adjacent-points [v]
  (->> (combo/selections [0 -1 1] (count v))
       (remove #{(repeat (count v) 0)})
       (map (fn [point-a point-b]
              (mapv + point-a point-b))
            (repeat v))))

(defn map-invert-to-sets
  "Inverts {:a :b :c :b} => {:b #{:a :c}}"
  [m]
  (reduce
   (fn [new-m [k v]]
     (update
      new-m
      v
      (fnil conj #{})
      k))
   {}
   m))

(defn scan-and-update
  [active-points]
  (let [neighbor-count->points
        (map-invert-to-sets
         (frequencies
          (mapcat
           adjacent-points
           active-points)))]
    (set/union
     (set/intersection
      (neighbor-count->points 2)
      active-points)
     (neighbor-count->points 3))))

(defn parse-line [y line]
  (->> line
       (map-indexed vector)
       (filter #(= \# (second %)))
       (map (fn [[idx _]]
              [idx y]))))

(defn parse-lines [lines]
  (->> lines
       (map-indexed vector)
       (mapcat #(parse-line (first %) (second %)))))

(defn input [file]
  (->> (util/input-lines file)
       (parse-lines)
       (map (fn [[x y]]
              [x y 0]))
       set))

(defn part-1 []
  (count
   (nth
    (iterate scan-and-update (input "day17-input.txt"))
    6)))

(defn part-2 []
  (count
   (nth
    (->> (input "day17-input.txt")
         (map (fn [[x y z]]
                [x y z 0]))
         set
         (iterate scan-and-update))
    6)))
