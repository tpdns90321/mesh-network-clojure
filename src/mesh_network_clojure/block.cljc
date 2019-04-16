(ns mesh-network-clojure.block)

;block header and block structure
(defrecord block-header [prev timestamp merkle auth])
(defrecord block [header transactions])
