(ns day15
  (:require [util]))

(defn last-n [state]
  (:last-n state))

(defn last-time [state n]
  (get-in state [:n->turn n]))

(defn next-state
  [state]
  (let [last-n (:last-n state)
        last-time (get-in state [:n->turn last-n])
        current-time (:current-time state)
        starting (first (:starting-numbers state))]
    (-> state
        (update :n->turn assoc last-n (:current-time state))
        (update :current-time inc)
        (update :starting-numbers rest)
        (assoc :last-n (if starting
                         starting
                         (if last-time
                           (- current-time last-time)
                           0))))))

(next-state {:last-n 6
             :n->turn {0 1
                       3 2}
             :current-time 3})

(defn states [starting-numbers]
  (iterate next-state
           {:last-n nil
            :current-time 0
            :starting-numbers starting-numbers
            :n->turn {}}))


(:last-n (nth (states [14,1,17,0,3,20]) 30000000))

(defn part-1 []
  (util/input-lines "day15-example.txt"))
