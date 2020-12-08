(ns day8
  (:require [util]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn step [{:as computer
             :keys [current-idx instructions]}]
  (let [[instruction n] (get instructions current-idx)]
    (case instruction
      :nop (update computer :current-idx inc)
      :jmp (assoc computer :current-idx (+ current-idx n))
      :acc (-> computer
               (update :current-idx inc)
               (update :accumulator + n)))))

(defn parse-line [s]
  (let [[inst n] (str/split s #" ")]
    [(keyword inst) (edn/read-string n)]))

(defn input-computer []
  {:instructions (->> (util/input-lines "day8-input.txt")
                      (mapv parse-line))
   :current-idx 0
   :accumulator 0})

(defn computer-versions
  "Given a computer with some instruction set, returns all possible versions of
  that computer that satisfy the rules given (a nop is swapped for a jmp or vice
  versa)"
  [computer]
  (let [swaps {:nop :jmp :jmp :nop}]
    (for [[n [inst v]] (map-indexed vector (:instructions computer))
          :when (contains? swaps inst)]
      (update computer :instructions assoc n [(inst swaps) v]))))

(defn run-until-stop
  "Returns either `nil` (if the computer never stops) or the value of the
  accumulator on termination"
  [computer]
  (->> computer
       (iterate step)
       (reduce (fn [seen {:keys [current-idx accumulator instructions]}]
                 (cond
                   (contains? seen current-idx)
                   (reduced nil)

                   (= current-idx (count instructions))
                   (reduced accumulator)

                   :else
                   (conj seen current-idx)))
               #{})))

(defn part-1 []
  (->> (input-computer)
       (iterate step)
       (reduce (fn [seen {:keys [current-idx accumulator]}]
                 (if (contains? seen current-idx)
                   (reduced accumulator)
                   (conj seen current-idx)))
               #{})))

(defn part-2 []
  (->> (input-computer)
       (computer-versions)
       (keep run-until-stop)))
