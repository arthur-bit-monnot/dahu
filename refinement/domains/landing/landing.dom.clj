
(define-discrete-state
  ^bool falling
;  ^real q
  ^real g
  ^real M_min
  ^real ISP
  ^real d_final
  ^real v_margin
  ^real d_margin
  )
(define-continuous-state
  ^real d ; distance
  ^real M ; mass which relates to the fuel level
  )

(define v (fst-deriv d))
(define a (snd-deriv d))
(define q (fst-deriv M)) ; fuel burning

(define not-crashed (< d d_final))
(define fuel-limit (> M M_min))
(define max-thrust (and (<= 0 q) (<= q 50)))
(define falling-dynamics
  (implies falling (= a (* dt (- g (* (* ISP g) (/ q M)))))))
(define falling-dynamics
  (implies falling
           (= a
              (* dt
                 (* g
                    (- 1
                       (* ISP (/ q M))))))))


(define landing
  (implies
   (not falling)
   (and
    (> d (- d_final d_margin))
    (< v v_margin)
    )))


