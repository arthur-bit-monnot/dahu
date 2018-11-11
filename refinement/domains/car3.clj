

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

(define running-until-goal
  (implies (not running)
           (>= distance 30)))

(define still-when-not-running
  (implies (not running)
           (= speed 0)))



(define constraints
  (list in-limits
        not-blown
        running-until-goal
        still-when-not-running
        ))


(define discrete-state-trajectory
  (list (dstate true 10 0) ; engine running
        (dstate false 10 0) ; engine not running
        ))
