(ns day7
  (:require [clojure.java.io :as io]
            [util]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-color [s]
  (keyword (str/replace s #" " "-")))

(defn parse-contained-bag [s]
  (when-let [[_ n color] (re-matches #"(\d+) (.+) bags?" s)]
    [(parse-color color) (edn/read-string n)]))

(defn parse-line [s]
  (let [[_ bag-color contents] (re-matches #"(.+) bags contain (.+)\." s)
        contained-bags (into {} (map parse-contained-bag (str/split contents #", ")))]
    [(parse-color bag-color) contained-bags]))

(defn parse-lines [strs]
  (into {} (map parse-line strs)))

(def contains-color?
  (memoize
   (fn [rules desired color]
     (or (= desired color)
         (let [contained-bags (get rules color)]
           (or (contains? contained-bags color)
               (some #(contains-color? rules desired %) (keys contained-bags))))))))

(defn count-bags [rules m]
  (apply +
         1
         (for [[color count] m]
           (* count (count-bags rules (get rules color))))))

(defn count-bags-inside [rules m]
  (dec (count-bags rules m)))

(defn part-1 []
  (let [rules (parse-lines (util/input-lines "day7-input.txt"))]
    (dec (count (filter #(contains-color? rules :shiny-gold %) (keys rules))))))

(defn part-2 []
  (let [rules (parse-lines (util/input-lines "day7-input.txt"))]
    (count-bags-inside rules (:shiny-gold rules))))
