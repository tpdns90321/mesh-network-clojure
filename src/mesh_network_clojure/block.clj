(ns mesh-network-clojure.block)

(defstruct block-header :prev :timestamp :merkle :auth)
(defstruct block :header :transactions)
