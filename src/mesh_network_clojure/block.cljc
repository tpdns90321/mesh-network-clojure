(ns mesh-network-clojure.block
  (:require [schema.core :as s]
            [mesh-network-clojure.crypto.hash :as hash]))

;block header and block structure
(def block-auth (s/pred :auth-type))
(s/defrecord block-header [prev :- hash/hash-type
                           timestamp
                           merkle :- [hash/hash-type]
                           auths :- (s/maybe [block-auth])])
(s/defrecord block [header :- block-header transactions])
