(ns day16
  (:require [util]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-ticket [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn parse-field [s]
  (let [[_ field min1 max1 min2 max2] (re-matches #"([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)" s)]
    {:field field
     :ranges [[(Integer/parseInt min1) (Integer/parseInt max1)]
              [(Integer/parseInt min2) (Integer/parseInt max2)]]}))

(defn parse-input [lines]
  (let [[field-lines
         _your-ticket-header
         [my-ticket-line]
         _nearby-ticket-header
         nearby-tickets] (partition-by (fn [ln]
                                         (contains? #{"your ticket:"
                                                      "nearby tickets:"}
                                                    ln))
                                       (remove empty? lines))]
    {:fields (map parse-field field-lines)
     :my-ticket (parse-ticket my-ticket-line)
     :nearby-tickets (map parse-ticket nearby-tickets)}))

(defn has-invalid-value? [{:keys [ranges]} n]
  (not-any? (fn [[min max]]
              (<= min n max))
            ranges))

(defn has-always-invalid-value? [fields n]
  (when (every? #(has-invalid-value? % n) fields)
    n))

(defn ticket-has-always-invalid-value? [fields ns]
  (some #(has-always-invalid-value? fields %) ns))

(defn part-1 []
  (let [{:keys [fields nearby-tickets]}
        (parse-input (util/input-lines "day16-input.txt"))]
    (->> nearby-tickets
         (keep (partial ticket-has-always-invalid-value? fields))
         (apply +))))

(defn find-possible-fields [fields column]
  (set
   (map :field
        (remove
         (fn [field] (some #(has-invalid-value? field %) column))
         fields))))

(defn resolve-possibilities [possibilities-sets]
  (->> (map-indexed vector possibilities-sets)
       (sort-by (comp count second))
       (reduce (fn [m [idx pset]]
                 (let [me (set/difference pset (set (vals m)))]
                   (when-not (= (count me) 1)
                     (throw (ex-info "more than one possibility"
                                     {:pset pset
                                      :m m})))
                   (assoc m idx (first me))))
               {})))

(defn part-2
  []
  (let [{:keys [fields nearby-tickets my-ticket]}
        (parse-input (util/input-lines "day16-input.txt"))
        valid-tickets (remove
                       (partial ticket-has-always-invalid-value? fields)
                       nearby-tickets)
        columns (apply mapv vector valid-tickets)
        field->column (->> valid-tickets
                           (apply map vector)
                           (map (partial find-possible-fields fields))
                           resolve-possibilities
                           set/map-invert)]
    (apply * (->> field->column
                  (filter #(str/starts-with? (key %) "departure"))
                  vals
                  (map #(nth my-ticket %))))))
