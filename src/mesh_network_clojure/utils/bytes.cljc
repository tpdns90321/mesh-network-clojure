(ns mesh-network-clojure.utils.bytes
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.platform.bytes :only [bytes-to-data! bytes-to-big-integer]]
        [mesh-network-clojure.utils :only [bytes!]]))

(def int-size 32)
(def long-size 64)

(defn limit-length! [order limit data]
  (let [diff (- limit (count data))
        zero (repeat (short 0))]
    (if (>= diff 0)
      (condp = order
        :little-endian (concat data (take diff zero))
        :big-endian (concat (take diff zero) data))
      nil)))

(defn def-buffer-types
  [convert-fn size]
  (fn [order data]
    (domonad maybe-m
             [size (/ size 8)
              data (bytes! (limit-length! order size data))]
      (bytes-to-data! convert-fn size order (byte-array data)))))

(def bytes-to-int! (def-buffer-types #(.getInt %) int-size))
(def bytes-to-long! (def-buffer-types #(.getLong %) long-size))

(defn def-big-integer [size]
  (fn [order data]
    (domonad maybe-m
      [size (/ size 8)
      data (bytes! (limit-length! order size data))]
      (bytes-to-big-integer order data))))

(def bytes-to-i128! (def-big-integer 128))

(defn def-unsigned-types
  "java can't support unsigned type! but java support big types. So this implement is ram-inefficiency, and It work very well!"
  [converter-fn size]
  (fn [order data]
    (domonad maybe-m
             [size (/ size 8)
              data (limit-length! order size data)]
             (converter-fn order data))))

(def bytes-to-unsigned-int! (def-unsigned-types bytes-to-long! int-size))
(def bytes-to-unsigned-long! (def-unsigned-types bytes-to-i128! long-size))
