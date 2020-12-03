;; Initial ns declaration

(ns day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Define our utility functions

;; =parse-input= takes a string and returns a matrix (with infinite rows!)
;; representing the entire map.

;; =tree?= takes an input map (as returned above) and a point (x, y - origin at the
;; top left of the map) and returns either =true= (yep, it's a tree), =false=
;; (nope - not a tree), or =nil= (we're off the map).

(defn parse-input [s]
  (->> (str/split s #"\n")
       (mapv (fn [line] (cycle (map #(= \# %) line))))))

(defn tree? [input [x y]]
  (nth (nth input y nil) x nil))

;; Define the function to count trees for a slope

;; Start with an (infinite) sequence of points on our slope. Find all the trees on
;; the route (stopping once we're off the map). Count them!

(defn count-trees-for-slope [input [slope-x slope-y]]
  (->>
   (iterate (fn [[x y]]
              [(+ x slope-x) (+ y slope-y)])
            [0 0])
   (map (partial tree? input))
   (take-while #(not= nil %))
   (filter identity)
   (count)))

;; Define the slopes to check

(def slopes-to-check [[1 1] [3 1] [5 1] [7 1] [1 2]])
