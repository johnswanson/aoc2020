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

(defn get-input-2 [f]
  (let [[_ bus-ids-str] (util/input-lines f)]
    (map #(if (= % "x")
            %
            (Integer/parseInt %))
         (str/split bus-ids-str #","))))

(defn factors-starting-at [minimum n]
  (cond
    (< (Math/sqrt n) minimum) (if (= 1 n) [] [n])
    (zero? (mod n minimum)) (conj (factors-starting-at minimum (/ n minimum)) minimum)
    :else (recur (inc minimum) n)))

(defn prime-factors [n]
  (factors-starting-at 2 n))

#_(/ (apply * (remove #(= "x" %) (get-input-2 "day13-input.txt")))
   971)

#_(last (take-while #(< % 1000000000000) (iterate #(+ 971 %) 971)))

(defn take-while-distinct [coll]
  (reduce (fn [[seen coll] v]
            (if (contains? seen v)
              (reduced coll)
              [(conj seen v) (conj coll v)]))
          [#{} []]
          coll))

(defn distances [a b]
  (take-while-distinct
   (iterate
    #(mod (- % a) b)
    (dec b))))

(distances 3 5)

(distances 3 5)
(distances 5 13)
(some #(when (= (second %) 3)
         %) (map-indexed vector (distances 7 59)))
(* 50 7)
(mod 1068781 (* 51 7))
(mod )

(some #(when (zero? (second %))
         (* (first %) 7))
      (map-indexed vector (distances 7 13)))

(let [n->index
      (dissoc
       (reduce (fn [m n]
                 (if (= n "x")
                   (update m :idx inc)
                   (-> m
                       (update :idx inc)
                       (assoc n (:idx m)))))
               {:idx 0}
               (get-input-2 "day13-input.txt"))
       :idx)]
  (reduce
   (fn [v [a b]]
     ())
   (partition 2 1 (keys n->index)))
  (prn (take 5 (distances (first (keys n->index))
                          (second (keys n->index)))))

  )
