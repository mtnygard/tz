(ns ft.reliability
  (:use (incanter core stats))
  (:import [java.lang Math]))

(defn constant-hazard-r [r]
  "Return a function that evaluates to the reliability of a constant hazard part"
  (fn [t] (exp (-  (* r t)))))

(defn series-r
  ([x]
     (fn [t] (x t)))
  ([x y]
     (fn [t] (* (x t) (y t))))
  ([x y & xs]
     (let [xs (list* x y xs)]
       (fn [t] (reduce #(* %1 (%2 t)) 1.0 xs)))))

(defn parallel-r
  ([x]
     (fn [t] (x t)))
  ([x y]
     (fn [t] (- 1.0 (* (- 1.0 (x t))
                       (- 1.0 (y t))))))
  ([x y & xs]
     (let [xs (list* x y xs)]
       (fn [t] (- 1.0 (reduce #(* %1 (- 1.0 (%2 t))) 1.0 xs))))))

(defn sample-availability [s t n]
  "Return a sequence of observed availability trials for time t. States will be true if still availble, false if the system has failed prior to this time. Repairs are not considered."
  (let [survival-odds (s t)]
    (map #(> survival-odds %) (sample-uniform n))))
