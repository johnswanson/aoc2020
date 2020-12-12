(ns day12
  (:require [util]))

(defn action [s]
  (first s))

(defn distance [s]
  (Integer/parseInt (apply str (rest s))))

(def initial-position {:x 0 :y 0 :a 90})

(def action->transform
  {\N #(update %1 :y + %2)
   \S #(update %1 :y - %2)
   \E #(update %1 :x + %2)
   \W #(update %1 :x - %2)
   \L #(-> %1
           (update :a - %2)
           (update :a mod 360))
   \R #(-> %1
           (update :a + %2)
           (update :a mod 360))
   \F (fn [{:as m :keys [a x y]} d]
        (case a
          90 (update m :x + d)
          180 (update m :y - d)
          270 (update m :x - d)
          0 (update m :y + d)))})

(defn move [a->t pos s]
  (let [a (action s)
        d (distance s)
        transform (a->t a)]
    (transform pos d)))

(defn part-1 []
  (let [{:keys [x y]} (reduce (partial move action->transform) initial-position (util/input-lines "day12-input.txt"))]
    (+ (Math/abs x) (Math/abs y))))

(defn rotate-right-90 [[x y]]
  [y (- x)])

(defn rotate-around-origin [a [x y]]
  (nth (iterate rotate-right-90 [x y])
       (/ (mod a 360) 90)))

(defn rotate-around-point [a [x y] [x1 y1]]
  (let [[x' y'] (rotate-around-origin a [(- x1 x) (- y1 y)])]
    [(+ x' x) (+ y' y)]))

(def action->transform-2
  {\N #(update-in %1 [:waypoint :y] + %2)
   \S #(update-in %1 [:waypoint :y] - %2)
   \E #(update-in %1 [:waypoint :x] + %2)
   \W #(update-in %1 [:waypoint :x] - %2)
   \L (fn [loc a]
        (let [ship [(get-in loc [:ship :x])
                    (get-in loc [:ship :y])]
              wp [(get-in loc [:waypoint :x])
                  (get-in loc [:waypoint :y])]
              [wp-x wp-y] (rotate-around-point (- a) ship wp)]
          (assoc loc :waypoint {:x wp-x :y wp-y})))
   \R (fn [loc a]
        (let [ship [(get-in loc [:ship :x])
                    (get-in loc [:ship :y])]
              wp [(get-in loc [:waypoint :x])
                  (get-in loc [:waypoint :y])]
              [wp-x wp-y] (rotate-around-point a ship wp)]
          (assoc loc :waypoint {:x wp-x :y wp-y})))
   \F (fn [loc times]
        (nth
         (iterate (fn [{:keys [ship waypoint] :as m}]
                    (let [new-waypoint
                          {:x (- (* 2 (:x waypoint))
                                 (:x ship))
                           :y (- (* 2 (:y waypoint))
                                 (:y ship))}]
                      (-> m
                          (assoc :ship waypoint)
                          (assoc :waypoint new-waypoint))))
                  loc)
         times))})

(def initial-pos-2 {:waypoint {:x 10 :y 1}
                    :ship {:x 0 :y 0}})

(defn part-2 []
  (let [{{x :x y :y} :ship}
        (reduce (partial move action->transform-2) initial-pos-2 (util/input-lines "day12-input.txt"))]
    (+ (Math/abs x)
       (Math/abs y))))

