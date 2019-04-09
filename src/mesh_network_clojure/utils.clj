(ns mesh-network-clojure.utils
  (:import [java.nio ByteBuffer ByteOrder])
  (:use [clojure.algo.monads :only [domonad maybe-m]]))

(defn bytes-to-data!
  "convert bytes seq or array to custom type that using convert-fn"
  [convert-fn size ^ByteOrder order data]
  (let [buff (doto (ByteBuffer/allocate size)
                   (.order order)
                   (.put data)
                   (.flip))]
    (convert-fn buff)))

(defn bytes! [data]
  (if (and (seq? data) (every? #(< % 256) data)) data nil))

(defn limit-length! [^ByteOrder order limit data]
  (let [diff (- limit (count data))
        zero (repeat (short 0))]
    (if (>= diff 0)
      (condp = order
        ByteOrder/LITTLE_ENDIAN (concat data (take diff zero))
        ByteOrder/BIG_ENDIAN (concat (take diff zero) data))
      nil)))

(defn def-buffer-types
  [convert-fn size]
  (fn [^ByteOrder order data]
    (domonad maybe-m
             [size (/ size 8)
              data (bytes! (limit-length! order size data))]
      (bytes-to-data! convert-fn size order (byte-array data)))))

(defn to-big-endian [^ByteOrder order data]
  (cond
    (= order ByteOrder/LITTLE_ENDIAN) (reverse data)
    (= order ByteOrder/BIG_ENDIAN) data))

(def bytes-to-int! (def-buffer-types #(.getInt %) Integer/SIZE))
(def bytes-to-long! (def-buffer-types #(.getLong %) Long/SIZE))

(defn def-big-integer [size]
  (fn [^ByteOrder order data]
    (domonad maybe-m
             [size (/ size 8)
              data (bytes! (limit-length! order size data))]
             (biginteger (byte-array (to-big-endian order data))))))

(def bytes-to-i128! (def-big-integer 128))

(defn def-unsigned-types
  "java can't support unsigned type! but java support big types. So this implement is ram-inefficiency, and It work very well!"
  [converter-fn size]
  (fn [^ByteOrder order data]
    (domonad maybe-m
             [size (/ size 8)
              data (limit-length! order size data)]
             (converter-fn order data))))

(def bytes-to-unsigned-int! (def-unsigned-types bytes-to-long! Integer/SIZE))
(def bytes-to-unsigned-long! (def-unsigned-types bytes-to-i128! Long/SIZE))

(defn slice [ar start end]
  (let
    [front (first (split-at end ar))]
    (last (split-at start front))))
