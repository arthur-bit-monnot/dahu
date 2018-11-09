

(defstruct dstate ^real up-limit ; upper limit on accelleration
                  ^real down-limit ; lower limit on acceleration
                  ^bool running ; true if the engine is running
)

(defstruct cstate ^real d ; distance traveled
)




(defn first-derivative [f s1 s2 dt] (/ (- (f s2) (f s1)) dt))
(defn second-derivative [f s1 s2 s3 dt1 dt2]
  (* 2 (/ (- ((first-derivative f) s2 s3 dt2) ((first-derivative f) s1 s2 dt1)) (+ dt1 dt2))))

(define speed (first-derivative cstate.d))
(define acceleration (second-derivative cstate.d))

(defn in-limits [ds cs1 cs2 cs3 dt1 dt2]
  (and
   (< (acceleration cs1 cs2 cs3 dt1 dt2) (dstate.up-limit ds))
   (> (acceleration cs1 cs2 cs3 dt1 dt2) (dstate.down-limit ds))
   ))


; condition for not blowing up the engine
(defn not-blown [ds cs1 cs2 cs3 dt1 dt2]
  (implies
   (dstate.running ds)
   (or (<= (acceleration cs1 cs2 cs3 dt1 dt2) 0)
       (< (speed cs1 cs2 dt1) 100))))

(defn running-until-goal [ds cs]
  (implies (not (dstate.running ds))
           (>= (cstate.d cs) 30)))

(defn still-when-not-running [ds cs1 cs2 dt]
  (implies (not (dstate.running ds))
           (= (speed cs1 cs2 dt) 0)))



(define running-state (dstate 10 0 true))
(define goal-state (dstate 10 0 false))

; functions that takes a function (a -> b -> c) and transforms it into (a -> [b] -> c
(defn always [f] (fn [a] (ref.forall (f a))))
(defn atstart [f] (fn [a] (ref.forfirst (f a))))
(defn atend [f] (fn [a] (ref.forlast (f a))))


(define discrete-state-trajectory (list running-state goal-state))
(define constraints0 (list
                      (always running-until-goal)
                      (atstart (fn [ds cs] (implies (dstate.running ds) (= (cstate.d cs) 0))))
                      ))
(define constraints1 (list (always still-when-not-running)))
(define constraints2 (list
                      (always not-blown)
                      (always in-limits)
                      ))

(defn specialize [dstate constraint] (constraint dstate))
(defn specialize-all [constraints dstate] (map (specialize dstate) constraints))

(defn constraints-by-state [constraints] (map (specialize-all constraints) discrete-state-trajectory))


