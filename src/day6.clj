(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn part-1 [groups]
  (->> groups
       (map (comp count set str/join))
       (apply +)))

(defn all-yes [group]
  (->> group
       (mapcat set)
       frequencies
       (keep (fn [[char num]]
               (when (= num (count group))
                 char)))))

(defn part-2 [groups]
  (->> groups
       (map all-yes)
       (map count)
       (apply +)))

(comment
  (def groups (->> "day6-input.txt"
                   io/resource
                   io/reader
                   line-seq
                   (partition-by empty?)
                   (remove (comp empty? first))))

  (part-1 groups)
  (part-2 groups))
