(ns mesh-network-clojure.block)

;block header and block structure
(defstruct block-header :prev :timestamp :merkle :auth)
(defstruct block :header :transactions)
