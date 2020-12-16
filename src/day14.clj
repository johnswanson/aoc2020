(ns day14
  (:require [util]
            [clojure.string :as str]))

(defn str->and-mask [s]
  (read-string
   (str
    "2r"
    (str/replace s #"X" "1"))))

(defn str->or-mask [s]
  (read-string
   (str
    "2r"
    (str/replace s #"X" "0"))))

(defn replace-char [s n c]
  (str (subs s 0 n) c (subs s (inc n))))

(defn replace-single-x
  "Returns a coll of masks"
  [s]
  (let [[loc _] (->> s
                     (map-indexed vector)
                     (some #(when (= \X (second %))
                              %)))]
    (for [val [\0 \1]]
      (replace-char s loc val))))

(defn str->float-masks-recur [s]
  (if-not (some #(= \X %) s)
    [s]
    (mapcat str->float-masks-recur (replace-single-x s))))

(defn replace-ones-and-zeroes-with-zs [s]
  (-> s
      (str/replace #"1" "A")
      (str/replace #"0" "Z")))

(defn replace-zs-with-xs [s]
  (-> s
      (str/replace #"A" "1")
      (str/replace #"Z" "X")))

(defn str->float-masks [s]
  (->> s
       (replace-ones-and-zeroes-with-zs)
       (str->float-masks-recur)
       (map replace-zs-with-xs)))

(defn apply-mask [mask v]
  (bit-and (str->and-mask mask)
           (bit-or (str->or-mask mask)
                   v)))

(defn parse-line [s]
  (let [[_match? cmd arg value] (re-matches #"(mask|mem?\[(\d+)\]) = (.+)" s)]
    (if (= cmd "mask")
      [:mask value]
      [:mem (Long/parseLong arg) (Long/parseLong value)])))

(defn execute-line [{:keys [addrs mask] :as m} s]
  (let [[cmd & args] (parse-line s)]
    (case cmd
      :mem (update m :addrs assoc (first args) (apply-mask mask (second args)))
      :mask (assoc m :mask (first args)))))

(defn part-1 []
  (->> (util/input-lines "day14-input.txt")
       (reduce (fn [state line]
                 (execute-line state line))
               {:addrs {}
                :mask ""})
       :addrs
       vals
       (apply +)))

(defn mask-mem-locs [mask mem-loc]
  (let [masks (str->float-masks mask)]
    (map #(apply-mask % mem-loc) masks)))

(defn execute-mem [{:keys [addrs mask]}
                   mem-loc
                   val]
  (reduce (fn [addrs mem-loc]
            (assoc addrs mem-loc val))
          addrs
          (mask-mem-locs mask mem-loc)))

(defn execute-line-p2 [{:keys [addrs mask] :as m} [cmd & args]]
  (case cmd
    :mem (assoc m :addrs (execute-mem m (first args) (second args)))
    :mask (assoc m :mask (first args))))

(defn part-2 []
  (->> (util/input-lines "day14-input.txt")
       (map parse-line)
       (reduce execute-line-p2
               {:addrs {} :mask ""})
       :addrs
       vals
       (apply +)))

(part-2)

#_(comment


;; (str->sloat-masks "X") =>   [[0] [1]]
;; (str->float-masks "X1") =>  [[0 1] [1 1]]
;; (str->float-masks "XX1") => [[0 0 1] [0 1 1]
;;                              [1 0 1] [1 1 1]]

(defn without-xs [v]
  (let [is-x? #(= (second %) \X)]
    (if-not (contains? (set v) \X)
      v
      (for [[loc _] (->> v
                         (map-indexed vector)
                         (filter is-x?))
            xval [\0 \1]
            :let [out (assoc v xloc xval)]]
        (without-xs out)))))

(defn mask->xs? [mask]
  (mapv #(= \X %) mask))

(mask->xs? "1XX0")

(defn xs->mask [v]
  (apply str (map (fn [v]
                    (if v
                      "X"
                      "X")))))

(defn ->float* [mask]
  (if-not (contains? (set mask) true)
    [mask]
    (mapcat ->float*
            (for [[xloc _] (->> mask
                                (map-indexed vector)
                                (filter #(true? (second %))))
                  :let [_ (prn xloc)]
                  xval [\0 \1]
                  :let [out (assoc mask xloc xval)]]
              out))))

(defn ->float [mask]
  (let [new-mask (mask->xs? mask)]
    (->> (->float* new-mask)
         (map (fn [v] (map #(if (false? %)
                              \X
                              %)
                           v)))
         (map #(apply str %)))
    ))

(->float "X11")

(defn apply-floater-mask [mask v]
  (bit-and (->float mask)
           ))
(->float "X01XX")

(part-1)

(parse-line "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
(parse-line "mem[8] = 11")


(let [and-mask
      or-mask (read-string
               (str
                "2r"
                (str/replace "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" #"X" "0")))]
  (bit-or
   or-mask
   (bit-and and-mask
            0)))

(bit-and 2r111111111111111111111111111111111101
         4)

(comment

  11
  (bit-and 11 )
  #_(let [s
          "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
          n1 (->> (str/replace s #"X" "1")
                  )]
      )


  (bit-and ))

  )
