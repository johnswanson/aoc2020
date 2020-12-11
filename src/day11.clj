(ns day11
  (:require [util]
            [clojure.math.combinatorics :as combo]))

(defn parse-char [c]
  (case c
    \L :empty
    \. :floor
    \# :occupied))

(defn parse-line [s]
  (mapv parse-char s))

(defn read-input [file]
  (mapv parse-line (util/input-lines file)))

(defn xdim [imap] (count (first imap)))
(defn ydim [imap] (count imap))

(defn dimensions [imap]
  ((juxt xdim ydim) imap))

(defn seat->status [imap [x y]]
  (get-in imap [y x] :off-grid))

(defn adjacent-statuses
  [imap [x y]]
  (for [[x' y'] (->> (combo/selections [inc dec identity] 2)
                     (remove #(every? (partial = identity) %))
                     (map (fn [[fx fy]]
                            [(fx x) (fy y)])))
        :let [[x-dim y-dim] (dimensions imap)]
        :when (and (<= 0 x' (dec x-dim))
                   (<= 0 y' (dec y-dim)))]
    (seat->status imap [x' y'])))

(defn transition [imap [x y]]
  (let [adjacent-statuses (adjacent-statuses imap [x y])]
    (cond
      (and (= :empty (seat->status imap [x y]))
           (not-any? #{:occupied} adjacent-statuses))
      :occupied

      (and (= :occupied (seat->status imap [x y]))
           (<= 4 (->> adjacent-statuses (filter #{:occupied}) count)))
      :empty

      :else
      (seat->status imap [x y]))))

(defn find-visible-status [imap [x y] [fx fy]]
  (->> (iterate (fn [[x y]]
                  [(fx x) (fy y)])
                [x y])
       next
       (some (fn [[x y]]
               (let [status (seat->status imap [x y])]
                 (when-not (= :floor status)
                   status))))))

(defn visible-statuses [imap [x y]]
  (->> (combo/selections [inc dec identity] 2)
       (remove #(every? (partial = identity) %))
       (map #(find-visible-status imap [x y] %))))

(defn transition-visible [imap [x y]]
  (let [visible-statuses (visible-statuses imap [x y])]
    (cond
      (and (= :empty (seat->status imap [x y]))
           (not-any? #{:occupied} visible-statuses))
      :occupied

      (and (= :occupied (seat->status imap [x y]))
           (<= 5 (->> visible-statuses (filter #{:occupied}) count)))
      :empty

      :else
      (seat->status imap [x y]))))

(defn scan-line [transition-fn imap y]
  (mapv #(transition-fn imap [% y]) (range (xdim imap))))

(defn scan [transition-fn imap]
  (mapv (partial scan-line transition-fn imap) (range (ydim imap))))

(defn scan-until-unchanged [transition-fn imap]
  (->> imap
       (iterate (partial scan transition-fn))
       (partition 2 1)
       (some #(when (apply = %)
                (first %)))))

(defn part-1 []
  (->> (read-input "day11-input.txt")
       (scan-until-unchanged transition)
       flatten
       (remove #(not= :occupied %))
       count))

(defn part-2 []
  (->> (read-input "day11-input.txt")
       (scan-until-unchanged transition-visible)
       flatten
       (remove #(not= :occupied %))
       count))
