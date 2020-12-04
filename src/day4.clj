;; #+TITLE: Day4
;; #+PROPERTY: header-args:clojure :session day4 :tangle ../src/day4.clj :results silent :comments org


(ns day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; First, we define a function for parsing a single "word" into a kv pair.

(defmulti parse-value (fn [k v] k))
(defmethod parse-value :default [_ v]
  v)

(defmethod parse-value :birth-year [_ v]
  (edn/read-string v))
(defmethod parse-value :issue-year [_ v]
  (edn/read-string v))
(defmethod parse-value :expiration-year [_ v]
  (edn/read-string v))

(defmethod parse-value :height [_ v]
  (let [[match? n t] (re-matches #"(\d+)(\w+)" v)]
    {:value (when match? (edn/read-string n))
     :units (when match? t)}))

(defmethod parse-value :eye-color [_ v]
  (keyword v))

(def str->field
  {"byr" :birth-year
   "iyr" :issue-year
   "eyr" :expiration-year
   "hgt" :height
   "hcl" :hair-color
   "ecl" :eye-color
   "pid" :passport-id
   "cid" :country-id})

(defn parse-word [s]
  (let [[k v] (str/split s #":")]
    {(str->field k)
     (when v
       (parse-value (str->field k) v))}))

;; Next we define a parsing function that can handle an entire document - which may be multiple lines:

(defn parse-doc [s]
  (apply merge (map parse-word (str/split s #"[\n ]+"))))

;; Finally we define a function that parses an entire input string

(defn parse-str [s]
  (map parse-doc (str/split s #"\n\n")))

;; Define some requirements

(def required-fields #{:birth-year
                       :eye-color
                       :issue-year
                       :expiration-year
                       :height
                       :hair-color
                       :passport-id})

;; Define the function to check whether a doc is valid

(defn valid? [d]
  (every? #(contains? (set (keys d)) %) required-fields))



;; Answer part 1

(def input (slurp (io/resource "day4-input.txt")))
(count (filter valid? (parse-str input)))

;; Define a validity function for part 2

(defmulti valid-field? (fn [f _] f))
(defmethod valid-field? :birth-year [_ v]
  (and (integer? v) (<= 1920 v 2002)))
(defmethod valid-field? :issue-year [_ v]
  (and (integer? v) (<= 2010 v 2020)))
(defmethod valid-field? :expiration-year [_ v]
  (and (integer? v) (<= 2020 v 2030)))
(defmethod valid-field? :height [_ v]
  (case (:units v)
    "in" (<= 59 (:value v) 76)
    "cm" (<= 150 (:value v) 193)
    false))
(defmethod valid-field? :hair-color [_ v]
  (re-matches #"#[0-9a-f]{6}" v))
(defmethod valid-field? :eye-color [_ v]
  (contains? #{:amb :blu :brn :gry :grn :hzl :oth} v))
(defmethod valid-field? :passport-id [_ v]
  (re-matches #"[0-9]{9}" v))
(defn valid-document? [d]
  (every? #(and (get d %)
                (valid-field? % (get d %))) required-fields))

;; Answer Part 2

(valid-document?
 (parse-doc "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"))
(count (filter valid-document? (parse-str example)))
(count (filter valid-document? (parse-str input)))
