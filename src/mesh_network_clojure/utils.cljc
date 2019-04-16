(ns mesh-network-clojure.utils)

(defn bytes! [data]
  (if (and (seq? data) (every? #(and (number? %) (< % 256)) data)) data nil))

(defn slice [ar start end]
  (let
    [front (first (split-at end ar))]
    (last (split-at start front))))
