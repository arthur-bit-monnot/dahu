

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
   (< (cstate.a cs) (dstate.up-limit ds))
   (> (cstate.a cs) (dstate.down-limit ds))
   ))

(defn in-limits-NEW [ds cs1 cs2 cs3 dt]
  (and
   (< (acceleration cs1 cs2 cs3 dt1 dt2) (dstate.up-limit ds))
   (> (acceleration cs1 cs2 cs3 dt1 dt2)(dstate.down-limit ds))
   ))

(defn state-evol-valid [ds cs1 cs2 dt]
                            (and (= (- (cstate.v cs2) (cstate.v cs1))
                                    (* (cstate.a cs1) dt) )
                                 (= (- (cstate.d cs2) (cstate.d cs1))
                                    (* (cstate.v cs1) dt) )))

; condition for not blowing up the engine
(defn not-blown [ds cs]
                                (implies
                                 (dstate.running ds)
                                 (or (<= (cstate.a cs) 0)
                                        (< (cstate.v cs) 100))))
; i.e. : error = min error(a < 1), error(v < 100)

(defn running-until-goal [ds cs]
                                (implies (not (dstate.running ds))
                                         (and (= (cstate.v cs) 0)
                                                 (>= (cstate.d cs) 30))))
;(defvar ^bool XX)
;
;(defn running-until-goal [ds cs]
;                                (implies (not (:running ds))
;                                         XX))

(define running-state (dstate 10 0 true))
(define goal-state (dstate 10 0 false))

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
   (map timed-cstate.state (band.cont band)) ; all cstates in the band
   (f (band.ds band))
   ) ; partial evaluation of f on the discrete state, resulting in a function ^cstate -> bool
  )

(defn fulfills-evolution-constraint [f band]
  ; band: ^band
  ; f: ^dstate -> ^cstate -> ^cstate -> dt -> bool
  (forall-consecutive
   (band.cont band) ; all timed-cstates in the band
   (fn [tcs1 tcs2]
     ((f (band.ds band)) ; f specialized for the discrete part of the band
       (timed-cstate.state tcs1) ; first cstate
       (timed-cstate.state tcs2) ; second cstate
       (timed-cstate.dt tcs2)))  ; time difference between the two states
   ) ; partial evaluation of f on the discrete state, resulting in a function ^cstate -> bool
  )

(defn fulfills-deriv1-constraint [f' predicate band]
  ; band: ^band
  ; f': ^cstate -> ^cstate -> dt -> X
  ; predicate: ^dstate -> X -> bool
  (forall-consecutive
   (band.cont band) ; all timed-cstates in the band
   (fn [tcs1 tcs2]
     (predicate
       (band.ds band)
       (f'
        (timed-cstate.state tcs1) ; first cstate
        (timed-cstate.state tcs2) ; second cstate
        (timed-cstate.dt tcs2))))  ; time difference between the two states
   ) ; partial evaluation of f on the discrete state, resulting in a function ^cstate -> bool
  )

(define bands (list b1 b2))

; true if the last cstate of b1 is the same as the fist continuous state of b2
(defn continuous-evolution [b1 b2] (=
                                    (timed-cstate.state (last (band.cont b1)))
                                    (timed-cstate.state (first (band.cont b2)))))

;(fulfills-instantaneous-constraint in-limits)

(defn valid-init [cs] (and (= (cstate.d cs) 0)
                           (= (cstate.v cs) 0)
                           ))

(defn first-derivative [f s1 s2 dt] (/ (- (f s2) (f s1)) dt))
(defn second-derivative [f s1 s2 s3 dt1 dt2]
  (/ (- ((first-derivative f) s2 s3 dt2) ((first-derivative f) s1 s2 dt1)) (+ dt1 dt2)))

(defn speed (first-derivative cstate.d))
(defn accel (second-derivative cstate.d))

(defn fullfills-init-constraints [bands f] (f (timed-cstate.state (first (band.cont (first bands))))))

(define constraints
  (and
   (forall-consecutive bands continuous-evolution)
   (forall bands (fulfills-instantaneous-constraint in-limits))
   (forall bands (fulfills-instantaneous-constraint not-blown))
   (forall bands (fulfills-instantaneous-constraint running-until-goal))
   (forall bands (fulfills-evolution-constraint state-evol-valid))
   (fullfills-init-constraints bands valid-init)
;   (fulfills-instantaneous-constraint b1 in-limits)
;   (fulfills-instantaneous-constraint b2 in-limits)
;   (fulfills-instantaneous-constraint b1 not-blown)
;   (fulfills-instantaneous-constraint b2 not-blown)
;   (forall (map b1 :state) (in-limits running-state))
  ))
