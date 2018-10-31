(defrecord circle [^real x ^real y ^real r])
(defrecord point [^real x ^real y])
(defrecord polygon [points])

(defrecord cstate [x y theta])
(defrecord dstate [moving area])


(def area1 (circle 10 10 4))
(def obstacles (list
  (circle 10 10 3)
  (point 5 5)
  (polygon '(2 3) '(3 4) '(4 5))
))





(defn euclidian-dist [x1 y1 x2 y2] (sqrt (+ (pow 2 (- x2 x1))
                                            (pow 2 (- y2 y1)))))



(defn distance [shape cs] (cond
                           ((point? shape)
                             (euclidian-dist (:x shape) (:y shape) (:x cs) (:y cs)))
                           ((circle? shape)
                             (- (euclidian-dist (:x shape) (:y shape) (:x cs) (:y cs)) (:radius shape)))
                           ((polygon? shape)
                             (comment "A bit too involved to specify here"))
                           ))

; +: a -> a -> a   a in (int real)
; abs; a -> a      a in (int real)

;diff: a -> a -> a  a in (int real)
(defn diff [a b] (abs (- a b)))

(defn no-collision [cs] (forall obstacles (fn [obs] (> (distance obs cs) 0))))
(defn in-area [cs shape] (< (distance shape cs) 0))

(defn velocity [cs1 cs2 dt] (/
                             (euclidian-dist (:x cs1) (:y cs1) (:x cs2) (:y cs2))
                             dt))

(defn acceleration [cs1 cs2 cs3 dt1 dt2] (/
                                         (diff
                                          (velocity cs1 cs2 dt1)
                                          (velocity cs2 cs3 dt2))
                                         (+ dt1 dt2)))


; regardless of the discrete state, w should never be in collision with an obstacle
(add-instantaneous-constraint (fn [ds cs] (no-collision cs)))

; The rover should always be in the area required by the discrete state
(add-instantaneous-constraint (fn [ds cs]
                                (in-area cs (:area ds)) ))

; if the discrete state require us to not be moving then (x y theta) should remain the same
(add-evolution-constraint (fn [ds cs1 cs2 dt]
                            (implies
                             (not (:moving ds))
                             (and
                              (= (:x cs1) (:x cs2))
                              (= (:y cs1) (:x cs2))
                              (= (:theta cs1) (:theta cs2)))
                             )
                            ))

; whenever moving, velocity should remain below 1 m/s
(add-evolution-constraint (fn [ds cs1 cs2 dt]
                            (implies
                             (:moving ds)
                             (< (velocity cs1 cs2 dt) 1))))

; whenever moving acceleration should remain below .1 m/ss
(add-secondary-evolution-constraint (fn [ds cs1 cs2 cs3 dt1 dt2]
                                      (implies
                                       (:moving ds)
                                       (< (acceleration cs1 cs2 cs3 d1 dt2) 0.1))))