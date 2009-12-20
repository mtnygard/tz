(ns tz
  (:use ft.reliability))

(defn delta [f]
  (fn [x dx]
    (- (f (+ x dx))
       (f x))))
