

(defstruct dstate ^real up-limit ; upper limit on accelleration
                  ^real down-limit ; lower limit on acceleration
                  ^bool running ; true if the engine is running
)

(defstruct cstate ^real d ; distance traveled
)

(defstruct cstate1 ^real d ; distance traveled
  ^real v ; velocity
  )

(defstruct cstate2 ^real d ; distance traveled
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
   (< (cstate2.a cs) (dstate.up-limit ds))
   (> (cstate2.a cs) (dstate.down-limit ds))
   ))


; condition for not blowing up the engine
(defn not-blown [ds cs]
                                (implies
                                 (dstate.running ds)
                                 (or (<= (cstate2.a cs) 0)
                                        (< (cstate2.v cs) 100))))
; i.e. : error = min error(a < 1), error(v < 100)

(defn running-until-goal [ds cs]
                                (implies (not (dstate.running ds))
                                         (and (= (cstate1.v cs) 0)
                                                 (>= (cstate1.d cs) 30))))


(defn first-derivative [f s1 s2 dt] (/ (- (f s2) (f s1)) dt))
(defn second-derivative [f s1 s2 s3 dt1 dt2]
  (/ (- ((first-derivative f) s2 s3 dt2) ((first-derivative f) s1 s2 dt1)) (+ dt1 dt2)))

(define speed (first-derivative cstate.d))
(define acceleration (second-derivative cstate.d))

(defn fst-derive-state [cs1 cs2 dt]
  (cstate1
   (cstate.d cs2);(avg (cstate.d cs1) (cstate.d cs2))
   (speed cs1 cs2 dt)
   ))

(defn snd-derive-state [cs1 cs2 cs3 dt1 dt2]
  (cstate2
   (cstate.d cs2)
   (avg (speed cs1 cs2 dt1) (speed cs2 cs3 dt2))
   (acceleration cs1 cs2 cs3 dt1 dt2)
   ))

(defn states0 [band] (map timed-cstate.state (band.cont band)))

(defn states1 [band]
  (map-consecutive2
   (fn [ts1 ts2] (fst-derive-state
                  (timed-cstate.state ts1)
                  (timed-cstate.state ts2)
                  (timed-cstate.dt ts2)
                  ))
   (band.cont band))
  )

(defn states2 [band]
  (map-consecutive3
   (fn [ts1 ts2 ts3] (snd-derive-state
                      (timed-cstate.state ts1)
                      (timed-cstate.state ts2)
                      (timed-cstate.state ts3)
                      (timed-cstate.dt ts2)
                      (timed-cstate.dt ts3)
                      ))
   (band.cont band))
  )

(defn fulfills-instantaneous-constraint [f band]
  ; band: ^band
  ; f: ^dstate -> ^cstate -> bool
  (forall
   (states0 band) ; all cstates in the band
   (f (band.ds band))
   ) ; partial evaluation of f on the discrete state, resulting in a function ^cstate -> bool
  )

(defn fulfills-evolution-constraint [f band]
  ; band: ^band
  ; f: ^dstate -> ^cstate' -> bool
  (forall
   (states1 band) ; all timed-cstates in the band
   (fn [cs]
     (f (band.ds band) cs)
   )
  ))
(defn fulfills-evolution2-constraint [f band]
  ; band: ^band
  ; f: ^dstate -> ^cstate' -> bool
  (forall
   (states2 band) ; all timed-cstates in the band
   (fn [cs]
     (f (band.ds band) cs)
   )
  ))


(define running-state (dstate 10 0 true))
(define goal-state (dstate 10 0 false))

(define running-in-limits (in-limits running-state))
(define goal-in-limits (in-limits goal-state))

; decision variables representing sequences of timestamped continuous states
(defvar ^timed-cstates cs1)
(defvar ^timed-cstates cs2)

(define b1 (band running-state cs1))
(define b2 (band goal-state cs2))

(define bands (list b1 b2))


;
;; true if the last cstate of b1 is the same as the fist continuous state of b2
(defn continuous-evolution [b1 b2] (=
                                    (last (states2 b1))
                                    (first (states2 b2))))
;
;;(fulfills-instantaneous-constraint in-limits)
;
(defn valid-init [cs] (and (= (cstate1.d cs) 0)
                           (= (cstate1.v cs) 0)
                           ))


;
(defn fullfills-init-constraints [bands f] (f (first (states1 (first bands)))))
;
(define constraints
  (and
   (forall bands (fn [b] (forall (band.cont b) (fn [ts] (>= (timed-cstate.dt ts) 0.1)))))
   (forall-consecutive bands continuous-evolution)
   (forall bands (fulfills-evolution2-constraint in-limits))
;   (states2 b1)
   (forall bands (fulfills-evolution2-constraint not-blown))
   (forall bands (fulfills-evolution-constraint running-until-goal))
;   (forall bands (fulfills-evolution-constraint state-evol-valid))
   (fullfills-init-constraints bands valid-init)
  ))
