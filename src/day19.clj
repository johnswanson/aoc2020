(ns day19
  (:require [clojure.string :as str]
            [util]))

(defn parse-condition [s]
  (let [[char-match? char] (re-find #"\"(\w+)\"" s)
        [rule-match? rule] (re-find #"(\d+)" s)]
    (cond
      (= s "|") :split
      char-match? char
      rule-match? (Integer/parseInt rule))))

(defn parse-rule [s]
  (let [[rule-id-str rule-str] (str/split s #":")
        rule-id (Integer/parseInt rule-id-str)
        rules (->> (str/split rule-str #" ")
                   (remove empty?)
                   (map parse-condition)
                   (partition-by #(= :split %))
                   (remove #(= [:split] %)))]
    [rule-id rules]))

(defn input [f]
  (let [[rules _ messages] (partition-by #(= "" %) (util/input-lines f))]
    {:rules (map parse-rule rules)
     :messages messages}))

(defn matches? [rules rule m])

(defn rule->regexp [rules rule]
  (cond
    (string? rule)
    rule

    (= 8 rule)
    (format "(%s)+"
            (str/join "|" (map (partial rule->regexp rules) (get rules rule))))
    (= 11 rule)
    (format "(%s)"
            (str/join "|" (map (partial rule->regexp rules) (get rules rule))))

    (number? rule)
    (format "(%s)"
            (str/join "|" (map (partial rule->regexp rules) (get rules rule))))

    (sequential? rule)
    (str/join (map (partial rule->regexp rules) rule))))

(defn match? [regexp s]
  (re-matches regexp s))

(defn part-1 []
  (let [{:keys [rules messages]} (input "day19-example2.txt")
        r0 (re-pattern (rule->regexp (into {} rules)
                                      0))]
    (->> messages
         (filter #(re-matches r0 %))
         count)))

(defn ->cfg [rules-strs]
  (str/join "\n" (map rule->cfg rules-strs)))
