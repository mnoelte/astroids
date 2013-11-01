(ns astroids.draw
  (:require [quil.core :as quil]
            [clojure.math.numeric-tower :as math]))

(defn world [state]
  (quil/background 0)
  (dorun (map (partial apply quil/point) (:stars state)))
  state)

(defn ship [state]
  (let [r (+ (/ Math/PI 2) (get-in state [:player :angle]))
        [x y] (get-in state [:player :position])]
    (quil/with-translation [x y]
      (quil/with-rotation [r]
        (quil/triangle 0 0 5 -15 10 0))))
  state)

(defn len [v]
  (math/sqrt (reduce + (map #(math/expt % 2) v))))

(defn norm [v]
  (let [l (len v)]
    (into [] (map #(/ % l) v))))

(defn skpr [s v]
  (into [] (map #(* s %) v)))

(defn lasers [state]
  (doseq [{[x y] :position dv :velocity} (:lasers state)
          :let [vshape -3
                [dx dy] (skpr vshape (norm dv))]]
    (quil/line x y dx dy))
  state)

(defn asteroids [state]
  (doseq [{[x y] :position size :size} (:asteroids state)]
    (quil/with-translation [x y]
      (quil/with-rotation [(quil/radians (/ (quil/millis) 10))]
        (quil/rect 0 0 size size))))
  state)
