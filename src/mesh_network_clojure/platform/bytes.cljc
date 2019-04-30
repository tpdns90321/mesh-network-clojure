(ns mesh-network-clojure.platform.bytes
  (:use [clojure.algo.monads :only [domonad maybe-m]]))

#?(:clj (import [java.nio ByteOrder]))

; endian
(def endians {:big-endian #?(:clj ByteOrder/BIG_ENDIAN),
              :little-endian #?(:clj ByteOrder/LITTLE_ENDIAN)})

(defn endian! [order]
  (if (some #(= order %) (keys endians))
    order
    nil))

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
                      (.order (order endians))
                      (.put (byte-array data))
                      (.flip))]
       (convert-fn buff))))

#?(:clj (def get-int (fn [^ByteBuffer buf] (.getInt buf))))
#?(:clj (def get-long (fn [^ByteBuffer buf] (.getLong buf))))

#?(:clj
    (defn bytes-to-big-integer! [order data]
      (biginteger (byte-array (to-big-endian! order data)))))

(defn unsigned-bytes [arr]
 (map #(short (bit-and % 0xff)) arr))

#?(:clj
   (defn data-to-bytes!
     "convert custom type to bytes seq or array"
     [input-fn size order data]
     (let [buff (doto (ByteBuffer/allocate size)
                      (.order (order endians)))]
       (input-fn buff data)
       (.flip buff)
       (unsigned-bytes
         (.array buff)))))

#?(:clj (def set-int (fn [^ByteBuffer buf data] (.putInt buf data))))
#?(:clj (def set-long (fn [^ByteBuffer buf data] (.putLong buf data))))

#?(:clj
   (defn big-integer-to-bytes! [order data]
     (let [convert (.toByteArray (biginteger data))]
       (unsigned-bytes
         (to-big-endian! order
                         convert)))))
