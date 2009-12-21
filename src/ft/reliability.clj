(ns ft.reliability
  (:use (incanter core stats))
  (:import [java.lang Math]))

(defn constant-hazard-r [r]
  "Return a function that evaluates to the reliability of a constant hazard part"
  (fn [t] (exp (-  (* r t)))))

(defn series-r
  ([x]
     (fn [t] (x t)))
  ([x & xs]
     (let [xs (list* x xs)]
       (fn [t] (reduce #(* %1 (%2 t)) 1.0 xs)))))

(defn parallel-r
  ([x]
     (fn [t] (x t)))
  ([x & xs]
     (let [xs (list* x xs)]
       (fn [t] (- 1.0 (reduce #(* %1 (- 1.0 (%2 t))) 1.0 xs))))))

(defn units-surviving [r n p]
  "Probability of exactly r out of n units surviving. Probability of any unit surviving is p."
  (* (choose n r)
     (pow p r)
     (pow (- 1 p) (- n r))))

(defn r-of-n-identical [r n x]
  "Define a system where at least r of the n components must function. All
   components have identical reliability, given by x. When applicable, this is the
   preferred function, as it will be much faster than the general r-of-n."
  (fn [t]
    (sum (map #(units-surviving % n (x t)) (range r (inc n))))))

;(defn r-of-n [r xs]
;  "Define a system where at least r of the components defined by xs
;   must function. The xs can have different reliabilities."
;  
;)

(defn sample-availability [s t n]
  "Return a sequence of observed availability trials for time t. States will be true if still availble, false if the system has failed prior to this time. Repairs are not considered."
  (let [survival-odds (s t)]
    (map #(> survival-odds %) (sample-uniform n))))
