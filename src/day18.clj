(ns day18
  (:require [util]
            [clojure.edn :as edn]))

(declare apply-op)

(defn evaluate [expr]
  (if-not (sequential? expr)
    expr
    (reduce (fn [n [op expr]]
              (apply-op op n expr))
            (first expr)
            (partition-all 2 (next expr)))))

(defn apply-op [op n expr]
  (case op
    + (+ (evaluate n) (evaluate expr))
    * (* (evaluate n) (evaluate expr))))

(defn parse-line [s]
  (edn/read-string (format "(%s)" s)))

(defn part-1 []
  (->> (util/input-lines "day18-input.txt")
       (map parse-line)
       (map evaluate)
       (reduce +)))

(defn preprocess-coll [coll]
  (if (= 1 (count coll))
    (first coll)
    (let [[first-exp op & exps] coll
          with-stolen-exp (conj (next exps)
                                [first-exp op (first exps)])]
      (case op
        + (preprocess-coll with-stolen-exp)
        * [first-exp op (preprocess-coll exps)]))))

(defn preprocess* [v]
  (if (sequential? v)
    (preprocess-coll v)
    v))

(defn preprocess
  "Returns the expression, with the `+`ed expressions wrapped in parens."
  [v]
  (clojure.walk/postwalk preprocess* v))

(defn part-2 []
  (->> (util/input-lines "day18-input.txt")
       (map parse-line)
       (map preprocess)
       (map evaluate)
       (reduce +)))

