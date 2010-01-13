(ns examples.static-web-farm
  (:use (ft reliability)))

(defn n-identical
  "Build n identical components with hazard function returned by f."
  [n f]
  (map (fn [_] f)
       (range n)))

(defn server-farm
  "Build a farm of n identical servers. Assumes one server can handle the load.
    Assumes a load distributor with a 1% failure rate. Server failure rate only depends
    on time."
  [n]
  (series-r
   (constant-hazard-r 0.01)
   (apply parallel-r (n-identical n (constant-hazard-r 0.01)))))

; Suppose we've got 10 web servers handling image assets
(def static-web-farm (server-farm 10))

(def one-year-reliability (static-web-farm 1))
(def two-year-reliability (static-web-farm 2))
(def three-year-reliability (static-web-farm 3))

(defn server-farm-8-of-10
  "Build a farm of 10 identical servers. Assumes that at least 8 servers must survive to
     handle necessary load. Assumes a load distributor with 1% failure rate. Assumes a
     perfect network. Server failure rate only depends on time."
  []
  (series-r
   (constant-hazard-r 0.01)
   (r-of-n-identical 8 10 (constant-hazard-r 0.01))))

