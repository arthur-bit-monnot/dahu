(define-events starting ending)

(define-discrete-state
  ^bool running
  ^real up-limit
  ^real down-limit
  )
(define-continuous-state
  ^real distance
  )

; define speed and acceleration as first and second derivatives of distance
(define speed (fst-deriv distance))
; equivalent to
; (define speed (/ (- distance (previous distance)) dt))

(define acceleration (snd-deriv distance))
;(define acceleration (- (fst-deriv (next distance)) (fst-deriv distance)))


(define in-limits
  (and
   (< acceleration up-limit)
   (> acceleration down-limit)
   ))

(define not-blown
  (implies
   running
   (or (<= acceleration 0)
       (< speed 100))))

(define goal-reached
  (implies ending
           (and ;(= speed 0)
                (>= distance 30))))

(define still-when-not-running
  (implies (not running)
           (= speed 0)))

(define valid-init-dist
  (implies starting
           (= distance 0)

))

(define valid-init-speed
  (implies starting
                (= speed 0)
           ))


(define constraints
  (list in-limits
        not-blown
        goal-reached
   valid-init-dist
   valid-init-speed
        still-when-not-running
        ))


(define discrete-state-trajectory
  (list (dstate true 1 -1) ; engine running
        (dstate false 1 -1) ; engine not running
        ))

(define happenings
  (list (events true false)
        (events false false)
        (events false true)))

(defstruct band
  ^events start-events
  ^dstate dstate
  ^events end-events
  )


(define bands (map (fn [i] (band
              (seq.get happenings i)
              (seq.get discrete-state-trajectory i)
              (seq.get happenings (i+ i 1i))))
     (seq.indices discrete-state-trajectory)))

(define cfun
  (meta.as-lambda current-events
                  (meta.as-lambda current-discrete-state constraints))
  )

(define band-constraints
  (map
   (fn [b] (list
            (cfun (band.start-events b) (band.dstate b))
            (cfun NO_EVENTS (band.dstate b))
            (cfun (band.end-events b) (band.dstate b))
            ))
   bands))