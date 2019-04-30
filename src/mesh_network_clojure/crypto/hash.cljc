(ns mesh-network-clojure.crypto.hash
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.utils :only [bytes!]]
        [mesh-network-clojure.platform.bytes :only [unsigned-bytes]]
        [mesh-network-clojure.platform.hash :only [hashes]]))

(defn hash-function [hash-name data]
  (domonad maybe-m
           [:when (keyword? hash-name)
            hash-func (hash-name hashes)
            data (bytes! data)]
           (unsigned-bytes (hash-func data))))

(def sha2 (partial hash-function :sha2))
(def sha3-256 (partial hash-function :sha3-256))
