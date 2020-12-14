(ns day13
  (:require [util]
            [clojure.string :as str]))

(defn get-input [file]
  (let [[ts-str bus-ids-str] (util/input-lines file)
        ts (Integer/parseInt ts-str)
        bus-ids (map #(Integer/parseInt %) (remove #(= "x" %) (str/split bus-ids-str #",")))]
    {:timestamp ts :buses bus-ids}))

(get-input "day13-example.txt")

(defn leave? [ts id]
  (when (zero? (mod ts id))
    id))

(defn next-bus [start-ts bus-ids]
  (->> (iterate inc start-ts)
       (reduce (fn [_ ts]
                 (when-let [bus (some (partial leave? ts) bus-ids)]
                   (reduced (* bus (- ts start-ts))))))))

(defn part-1 []
  (let [{ts :timestamp
         buses :buses} (get-input "day13-input.txt")]
    (next-bus ts buses)))

(defn find-next-intersection
  [steps [between n]]
  (first
   (filter #(zero?
             (mod (+ between %) n))
           steps)))

(defn part-2 []
  (->> (str/split (second (util/input-lines "day13-input.txt")) #",")
       (map #(if (= % "x")
               %
               (Integer/parseInt %)))
       (map-indexed vector)
       (remove #(string? (second %)))
       (reduce
        (fn [{:keys [current multiples]} [dist n]]
          (let [steps (iterate (fn [v]
                                 (+ v (apply * multiples)))
                               current)
                next-int (find-next-intersection steps [dist n])]
            {:current next-int
             :multiples (conj multiples n)}))
        {:current 0
         :multiples []})
       :current))
