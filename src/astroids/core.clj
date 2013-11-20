(ns astroids.core
  (:require [astroids.draw :as draw]
            [astroids.state :as state]
            [quil.core :as quil]))

(def width 800)
(def height 400)

(def key-map {32 :space
              40 :down
              37 :left
              38 :up
              39 :right})

(def game (atom {:player {:position [(/ width 2) (/ height 2)]
                          :angle 0
                          :velocity [0 0]}
                  :lasers []  ; :position and velocity

                 :asteroids (take 3 (repeatedly (fn [] {:position [(rand-int width) (rand-int height)]
                                                       :velocity [(rand-int 10) (rand-int 10)]
                                                       :size 20})))
                 :stars (take 100 (repeatedly (fn [] [(rand-int width) (rand-int height)])))
                 :buttons #{}}))

(defn setup []
  (quil/smooth)
  (quil/frame-rate 60)
  (quil/sphere-detail 6)
  (quil/background 0)
  (quil/stroke 255)
  (quil/stroke-weight 2)
  (quil/fill 0)
  (quil/rect-mode :center))

(defn tick []
  (swap! game
         #(-> %
              state/world
              state/ship
              state/lasers
              state/asteroids))
  (-> @game
      draw/world
      draw/ship
      draw/lasers
      draw/asteroids
      ))

(defn press []
  (when-let [k (key-map (quil/key-code))]
    (swap! game (fn [g] (update-in g [:buttons] #(conj % k))))))

(defn release []
  (when-let [k (key-map (quil/key-code))]
    (swap! game (fn [g] (update-in g [:buttons] #(disj % k))))))

(quil/defsketch astroids
                :title "astroids"
                :size [width height]
                :setup setup
                :draw tick
                :key-pressed press
                :key-released release)
