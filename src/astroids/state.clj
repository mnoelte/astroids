(ns astroids.state)

(defn constant [ret]
  (fn [& args] ret)
  )

(alter-var-root #'*out* (constantly *out*))
(defn world [state]
  state)

(defn update [m path f]
  #_(prn "update")
  (assoc-in m path (f (get-in m path)))
  )

(defmacro deftrans [symb [name] ds]
  `(def ~symb (make-trans [~name] ~ds))
  )


(defmacro make-trans [[name] ds]
  (mapv (fn [[pred trans]]
         `[(fn [~name] ~pred)
           (fn [~name] ~trans)]) ds))

(deftrans trans  [state]
  [[((:buttons state) :right)  (update state [:player :angle]  #(+ 0.1 %))]
   [((:buttons state) :left) (update state [:player :angle]  #(+ 6.182 %))]
   [((:buttons state) :space) (update state [:lasers :angle]  #(+ 6.182 %))]
   [((:buttons state) :up)
    (assoc-in state [:player :velocity]
              (mapv + (mapv * [0.1 0.1] [(Math/cos (:angle (:player state)))
                                         (Math/sin (:angle (:player state)))])
                    (:velocity (:player state))))]])


#_(def trans  (make-trans [state]
                        [[((:buttons state) :left)  (update state [:player :angle]  #(+ 0.1 %))]
                         [((:buttons state) :space)
                          (assoc-in state [:player :velocity]
                                    (mapv + (mapv * [0.1 0.1] [(Math/cos (:angle (:player state)))
                                                               (Math/sin (:angle (:player state)))])
                                          (:velocity (:player state))))]]))

#_(def trans  (make-trans [[((:buttons state) :left)  (update state [:player :angle]  #(+ 0.1 %))]
                         [ ((:buttons state) :space)
                           (assoc-in state [:player :velocity]
                                     (mapv + (mapv * [0.1 0.1] [(Math/cos (:angle (:player state)))
                                                                (Math/sin (:angle (:player state)))])
                                           (:velocity (:player state))))]]))

#_(def trans [[(fn [state] ((:buttons state) :left)) (fn [state] (update state [:player :angle]  #(+ 0.1 %)))]
            [(fn [state] ((:buttons state) :space))
             (fn [state] (assoc-in state [:player :velocity]
                                  (mapv + (mapv * [0.1 0.1] [(Math/cos (:angle (:player state)))
                                                             (Math/sin (:angle (:player state)))])
                                        (:velocity (:player state)))))]
            ])

(defn do-trans [state trans]
  (let [erg (reduce (fn [actstate [pred trans]]
                      (if (pred actstate) (trans actstate) actstate))
                    state trans)]
    erg))

(defn set-velocity [state]
  (let [velocity (get-in state [:player :velocity])]

    (do-trans state trans)
    #_(if ((:buttons state) :left)
      (update state [:player :angle]  #(+ 0.1 %))
      (if ((:buttons state) :space)
          (assoc-in state [:player :velocity]
                    (mapv + (mapv * [0.1 0.1] [(Math/cos (:angle (:player state))) (Math/sin (:angle (:player state)))]) (:velocity (:player state))))
          state))))

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
