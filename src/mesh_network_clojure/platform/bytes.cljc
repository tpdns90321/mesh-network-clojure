(ns mesh-network-clojure.platform.bytes
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.utils :only [bytes!]]
        [mesh-network-clojure.platform :only [to-raw!]]))

(defn to-big-endian! [order data]
  (condp = order
    :little-endian (reverse data)
    :big-endian data
    nil))

(defn to-little-endian! [order data]
  (condp = order
    :big-endian (reverse data)
    :little-endian data
    nil))

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

(def get-int #?(:clj (fn [^ByteBuffer buf] (.getInt buf))))
(def get-long #?(:clj (fn [^ByteBuffer buf] (.getLong buf))))

#?(:clj
    (defn bytes-to-big-integer! [order data]
      (biginteger (byte-array (to-big-endian! order data)))))

#?(:clj
   (defn data-to-bytes!
     "convert custom type to bytes seq or array"
     [input-fn size order data]
     (let [buff (doto (ByteBuffer/allocate size)
                      (.order (to-raw! order)))]
       (input-fn buff data)
       (.flip buff)
       (.array buff))))

#?(:clj
   (defn big-integer-to-bytes! [order ^BigInteger data]
     (let [convert (.toByteArray data)]
       (map
         #(short (bit-and % 0xff))
         (to-big-endian! order 
                         convert)))))
