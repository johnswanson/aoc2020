(ns day1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (map #(Integer. %)
       (-> (io/resource "day1.input")
           slurp
           (str/split #"\n"))))

(defn answer [coll]
  (first
   (for [[i x] (map-indexed vector coll)
         [j y] (map-indexed vector coll)
         :when (and (< i j)
                    (= 2020 (+ x y)))]
     (* x y))))

(defn answer-pt-2 [coll]
  (first
   (for [[i x] (map-indexed vector coll)
         [j y] (map-indexed vector coll)
         [k z] (map-indexed vector coll)
         :when (and (< i j k)
                    (= 2020 (+ x y z)))]
     (* x y z))))
