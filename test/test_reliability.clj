(ns test-reliability
  (:use ft.reliability)
  (:use clojure.test))

(defn broken [t] 0.0)

(def never-fails (constant-hazard-r 0.0))

(deftest single-perfect-component
  (is (= 1.0 (never-fails 1.0)))
  (is (= 1.0 (never-fails 2.0))))

(deftest good-series
  (let [c1 never-fails
        c2 never-fails
        system (series-r c1 c2)]
    (is (= 1.0 (system 1.0)))
    (is (= 1.0 (system 2.0)))))

(deftest bad-series
  (let [c1 never-fails
        c2 broken
        system (series-r c1 c2)]
    (is (= 0.0 (system 1.0)))
    (is (= 0.0 (system 2.5)))))

(deftest realistic-series
  (let [c1 (constant-hazard-r 0.01)
        c2 (constant-hazard-r 0.001)
        c3 (constant-hazard-r 0.02)
        system (series-r c1 c2 c3)
        year-1-survival (system 1)
        year-2-survival (system 2)]
    (is (< 0.935 year-2-survival 0.940 0.965 year-1-survival 0.970))))