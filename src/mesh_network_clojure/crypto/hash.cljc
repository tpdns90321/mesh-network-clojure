(ns mesh-network-clojure.crypto.hash
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.utils.bytes :only [unsigned-bytes]]
        [mesh-network-clojure.platform.bytes :only [to-unsigned-bytes]]
        [mesh-network-clojure.platform.hash :only [hashes]])
  (:require [schema.core :as s]))

(def hash-keys (apply s/enum (keys hashes)))

(s/defn hash-function :- (s/maybe unsigned-bytes)
  [hash-name :- hash-keys
   data :- unsigned-bytes]
  (let [hash-func (hash-name hashes)
        res (to-unsigned-bytes (hash-func data))]
       (when-not (empty? res) res)))

(def sha2 (partial hash-function :sha2))
(def sha3-256 (partial hash-function :sha3-256))
