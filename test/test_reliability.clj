(ns test-reliability
  (:use ft.reliability)
  (:use clojure.test))

(defn broken [t] 0.0)

(def never-fails (constant-hazard-r 0.0))
(def c1 (constant-hazard-r 0.01))
(def c2 (constant-hazard-r 0.001))
(def c3 (constant-hazard-r 0.02))

(deftest single-perfect-component
  (is (= 1.0 (never-fails 1.0)))
  (is (= 1.0 (never-fails 2.0))))

(deftest good-series
  (let [system (series-r never-fails never-fails)]
    (is (= 1.0 (system 1.0)))
    (is (= 1.0 (system 2.0)))))

(deftest bad-series
  (let [system (series-r never-fails broken)]
    (is (= 0.0 (system 1.0)))
    (is (= 0.0 (system 2.5)))))

(deftest realistic-series
  (let [system (series-r c1 c2 c3)
        year-1-survival (system 1.0)
        year-2-survival (system 2.0)]
    (is (< 0.935 year-2-survival 0.940 0.965 year-1-survival 0.970))))

(deftest good-parallel
  (let [system (parallel-r broken never-fails)]
    (is (= 1.0 (system 1.0)))
    (is (= 1.0 (system 2.0)))))

(deftest bad-parallel
  (let [system (parallel-r broken broken)]
    (is (= 0.0 (system 1.0)))
    (is (= 0.0 (system 2.0)))))

(deftest realistic-parallel
  (let [system (parallel-r c1 c2 c3)]
    (is (< 0.99999 (system 2.0) 0.999999 (system 1.0) 1.0))))
