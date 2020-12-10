(ns day10
  (:require [util]
            [clojure.set :as set]))

(defn part-1 []
  (let [adapters (map #(Integer/parseInt %)
                      (util/input-lines "day10-input.txt"))
        adapters* (vec (sort (conj adapters 0)))
        adapters (conj adapters*
                       (+ 3 (last adapters*)))
        jolt-diffs (->> adapters
                        (partition 2 1)
                        (map #(- (second %) (first %)))
                        frequencies)]
    (* (get jolt-diffs 1)
       (get jolt-diffs 3))))

(def routes
  "Given a map of numbers to node-maps, and a particular node-map, return the
  total number of routes from this node-map to the end."
  (memoize
   (fn routes*
     [n->reachable n]
     (let [reachable (get n->reachable n)]
       (if (empty? reachable)
         ;; assume it's the last one
         1

         (reduce + (map (partial routes n->reachable) reachable)))))))

(defn part-2 []
  (let [adapters (map #(Integer/parseInt %) (util/input-lines "day10-input.txt"))
        adapter-set (conj (set adapters) 0)
        n->reachable (into {}
                           (for [n adapter-set]
                             [n (set/intersection adapter-set
                                                  (set (range (inc n) (+ n 4))))]))]
    (routes n->reachable 0)))
