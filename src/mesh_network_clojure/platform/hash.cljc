(ns mesh-network-clojure.platform.hash)

#?(:clj (import [org.apache.commons.codec.digest DigestUtils]))

; hash function
(def hashes {:sha2 #?(:clj (fn [data] (DigestUtils/sha256 (byte-array data)))),
             :sha3-256 #?(:clj (fn [data] (DigestUtils/sha3_256 (byte-array data))))})
