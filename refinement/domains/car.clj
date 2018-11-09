; Example problem definition for the car problem
; the problem is to make a car travel a given distance, subject to acceleration and velocity limits

; This is a low level definition that is self contained (and not tied to a PDDL/ANML definition)
; Once



;; Product types
; those define the structure of the continuous and discrete states, as well as the bands
; provided here for completeness but to be derived from the domain definition in PDDL/ANML

;; Discretely evolving state
(defstruct dstate ^real up-limit ; upper limit on accelleration (of type real)
                  ^real down-limit ; lower limit on acceleration (of type real)
                  ^bool running ; true if the engine is running (of type bool)
)

;; Continuously evolving state
(defstruct cstate ^real d ; distance traveled
                  ^real v ; velocity
                  ^real a ; acceleration
)

;; Timestamped continuous state
(defstruct timed-cstate ^cstate state
                        ^real   dt)

;; A band is a discrete state and a list of timedstamped continuous states
(defstruct band ^dstate ds
                ^timed-cstates cont  ; note the s at the end of timed-cstateS to denote a list (need better syntax)
  )




;;;;;;; Variables
; provided here to make things self contained.
; to be derived from the plan

; we define two bands, one in which the engine is running and one in which the engine is stoped

; (known) discrete state where the engine is runnning
(define running-state (dstate 10 0 true))
; variable representing a sequence of timestamped continuous states
(defvar ^timed-cstates cs1)
; band that combines the two
(define b1 (band running-state cs1))

; similarly, define a second band where the engine is not running
(define end-state (dstate 10 0 false))
(defvar ^timed-cstates cs2)
(define b2 (band end-state cs2))

; bands of the problem
; the two decision variables cs1 and cs2 are embedded in this expression
(define bands (list b1 b2))



;;;;; Syntax note

;; Example function definition with two parameters (a and b) that return true if b is below a
(defn below [a b] (>= a b))
; I use the following notation to document the type of the function
; below: ^real -> ^real -> ^ bool
; this can be interpreted as a function that takes to real parameters and return a boolean
(below 10 5) ; would return true
; the other interpretation is that it is a function whose parameter is a real and
; returns a function of type (^real -> ^bool)
; for instance you can define a new function by partially applying
(define below10 (below 10)) ; below10: ^real -> ^bool
(below10 5) ; returns true








;;;;; Functions for defining constraints on states

;; function that returns true if the if the acceleration in the continuous state cs
;; is in the limits specified by the given discrete state ds
;; in-limits: ^dstate -> ^cstate -> bool
(defn in-limits [ds cs]
  (and
   (< (cstate.a cs) (dstate.up-limit ds))
   (> (cstate.a cs) (dstate.down-limit ds))
   ))

; condition for not blowing up the engine
; if the engine is running (in the discrete state) then it should be the case
; that we are not accelerating or that the speed is below 100
;; not-blown: ^dstate -> ^cstate -> bool
(defn not-blown [ds cs]
  (implies
   (dstate.running ds)
   (or (<= (cstate.a cs) 0)
       (< (cstate.v cs) 100))))

; enforces that if the engine is stopped, we have traveled 30 meter (d >= 30)
; and the car does not moves (v = 0)
(defn running-until-goal [ds cs]
  (implies (not (dstate.running ds))
           (and (= (cstate.v cs) 0)
                (>= (cstate.d cs) 30))))


;;; Constraints on the first derivative.
;; takes as parameters a discrete state (ds), two consecutive continuous state (cs1 cs2)
; and a real (dt) that give the temporal separation between the two
;; Parameter types:
; ds: ^dstate
; cs1: ^cstate
; cs2: ^cstate
; dt: ^real
; return true if the evolution of speed and distance is consistent
(defn state-evol-valid [ds cs1 cs2 dt]
                            (and (= (- (cstate.v cs2) (cstate.v cs1))
                                    (* (cstate.a cs1) dt) )
                                 (= (- (cstate.d cs2) (cstate.d cs1))
                                    (* (cstate.v cs1) dt) )))


; true if the the given ^cstate is valid initial state
; valid-init: ^cstate -> ^bool
(defn valid-init [cs] (and (= (cstate.d cs) 0)
                           (= (cstate.v cs) 0)
                           ))





;;;;;; Helper functions
; those are defined in user space but will be provided by default

; True if all continuous states in the band fulfills the constraint denoted by f
(defn fulfills-instantaneous-constraint [f band]
  ; band: ^band
  ; f: ^dstate -> ^cstate -> bool
  ; returns ^bool
  (forall
   (map timed-cstate.state (band.cont band)) ; all cstates in the band
   (f (band.ds band)) ; partial evaluation of f on the discrete state, resulting in a function ^cstate -> bool
   )
  )

(defn fulfills-evolution-constraint [f band]
  ; band: ^band
  ; f: ^dstate -> ^cstate -> ^cstate -> dt -> bool
  (forall-consecutive
   (band.cont band) ; all timed-cstates in the band
   (fn [tcs1 tcs2] ; inline function that take two timestamped states
     (f (band.ds band) ; f applied to the discrete part of the band
        (timed-cstate.state tcs1)   ; first cstate
        (timed-cstate.state tcs2)   ; second cstate
        (timed-cstate.dt tcs2)))    ; time difference between the two states
   )
  )


; true if the last cstate of b1 is the same as the fist continuous state of b2
; continuous-evolution: ^band -> ^band -> ^bool
(defn continuous-evolution [b1 b2] (=
                                    (timed-cstate.state (last (band.cont b1)))
                                    (timed-cstate.state (first (band.cont b2)))))


; applies f on the first continuous state of the bands
(defn fullfills-init-constraints [bands f] (f (timed-cstate.state (first (band.cont (first bands))))))





;; Constraints in the problem
; conjunction of applying all constraints on all bands


(define constraints
  (and
   (forall bands (fulfills-instantaneous-constraint in-limits))
   (forall bands (fulfills-instantaneous-constraint not-blown))
   (forall bands (fulfills-instantaneous-constraint running-until-goal))
   (forall bands (fulfills-evolution-constraint state-evol-valid))
   (fullfills-init-constraints bands valid-init)
   (forall-consecutive bands continuous-evolution)
  ))


