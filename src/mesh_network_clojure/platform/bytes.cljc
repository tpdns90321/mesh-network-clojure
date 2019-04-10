(ns mesh-network-clojure.platform.bytes
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.utils :only [bytes!]]
        [mesh-network-clojure.platform :only [to-raw!]]))

#?(:clj (import [java.nio ByteBuffer]))

#?(:clj
   (defn bytes-to-data!
     "convert bytes seq or array to custom type that using convert-fn"
     [convert-fn size order data]
     (let [buff (doto (ByteBuffer/allocate size)
                      (.order (to-raw! order))
                      (.put data)
                      (.flip))]
       (convert-fn buff))))

(def get-int #?(:clj #(.getInt %)))
(def get-long #?(:clj #(.getLong %)))

(defn to-big-endian! [order data]
  (condp = order
    :little-endian (reverse data)
    :big-endian data
    nil))

#?(:clj
    (defn bytes-to-big-integer! [order data]
      (biginteger (byte-array (to-big-endian! order data)))))

