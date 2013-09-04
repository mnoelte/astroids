(ns astroids.state)

(defn world [state]
  state)

(defn set-velocity [state]
  (let [velocity (get-in state [:player :velocity])]
    (if ((:buttons state) :space)
      (assoc-in state [:player :velocity]
                (mapv + [0.1 0] (:velocity (:player state))))
      state)))

(defn move-ship [state]
  (let [velocity (get-in state [:player :velocity])
        position (get-in state [:player :position])]
    (assoc-in state [:player :position] (mapv + velocity position))))

(defn ship [state]
  (-> state
      move-ship
      set-velocity))

(defn lasers [state]
  state)

(defn asteroids [state]
  state)
