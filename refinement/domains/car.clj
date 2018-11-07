

(defstruct dstate ^real up-limit ; upper limit on accelleration
                  ^real down-limit ; lower limit on acceleration
                  ^bool running ; true if the engine is running
)

(defstruct cstate ^real d ; distance traveled
                  ^real v ; velocity
                  ^real a ; acceleration
)
(defstruct timed-cstate ^cstate state
                        ^real   dt)

(defstruct band ^dstate ds
                ^timed-cstates cont)

(defn avg [a b] (/ (+ a b) 2))

(defn in-limits [ds cs]
                                (and
                                 (< (:a cs) (:up-limit ds))
                                 (> (:a cs) (:down-limit ds))
                                 ))

(defn state-evol-valid [ds cs1 cs2 dt]
                            (and (= (- (:v cs2) (:v cs1))
                                    (* (:a cs1) dt) )
                                 (= (- (:d cs2) (:d cs1))
                                    (* (:v cs1) dt) )))

; condition for not blowing up the engine
(defn not-blown [ds cs]
                                (implies
                                 (:running ds)
                                 (or (<= (:a cs) 0)
                                        (< (:v cs) 100))))
; i.e. : error = min error(a < 1), error(v < 100)

(defn running-until-goal [ds cs]
                                (implies (not (:running ds))
                                         (and (= (:v cs) 0)
                                                 (>= (:d cs) 30))))


(define running-state (dstate 10.0 0.0 true))
(define goal-state (dstate 10.0 0.0 false))

(define running-in-limits (in-limits running-state))
(define goal-in-limits (in-limits goal-state))

; decision variables representing sequences of timestamped continuous states
(defvar ^timed-cstates cs1)
(defvar ^timed-cstates cs2)

(define b1 (band running-state cs1))
(define b2 (band goal-state cs2))

(defn fulfills-instantaneous-constraint [f band]
  ; band: ^band
  ; f: ^dstate -> ^cstate -> bool
  (forall
   (map :state (:cont band)) ; all cstates in the band
   (f (:ds band))) ; partial evaluation of f on the discrete state, resulting in a function ^cstate -> bool
  )

(define bands (list b1 b2))

; true if the last cstate of b1 is the same as the fist continuous state of b2
(defn continuous-evolution [b1 b2] (= (last (:cont b1)) (first (:cont b2))))

(define constraints
  (and
   (all-consecutive bands continuous-evolution)
   (forall bands (fulfills-instantaneous-constraint in-limits))
   (forall bands (fulfills-instantaneous-constraint not-blown))
;   (fulfills-instantaneous-constraint b1 in-limits)
;   (fulfills-instantaneous-constraint b2 in-limits)
;   (fulfills-instantaneous-constraint b1 not-blown)
;   (fulfills-instantaneous-constraint b2 not-blown)
;   (forall (map b1 :state) (in-limits running-state))
  ))
