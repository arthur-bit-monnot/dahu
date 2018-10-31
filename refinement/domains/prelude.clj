(defn > [a b] (< b a))
(defn >= [a b] (<= b a))

(defn - [a b] (+ a (neg b)))

(defn implies [a b] (or (not a) b))
(defn => [a b] (implies a b))

(defn compile [tpe f] ???)