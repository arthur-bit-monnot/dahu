

(defstruct dstate ^real up-limit ; upper limit on accelleration
                  ^real down-limit ; lower limit on acceleration
                  ^bool running ; true if the engine is running
)

(defstruct cstate ^real d ; distance traveled
                  ^real v ; velocity
                  ^real a ; acceleration
)

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


(define s (dstate 10.0 0.0 true))
(not-blown s)
(in-limits s)