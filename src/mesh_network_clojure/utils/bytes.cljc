(ns mesh-network-clojure.utils.bytes
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.platform.bytes
          :only [bytes-to-data! bytes-to-big-integer!
                 data-to-bytes! big-integer-to-bytes!
                 get-int get-long set-int set-long
                 endian! to-big-endian!]]
        [mesh-network-clojure.platform.utils :only [int-pow]])
  (:require [schema.core :as s]))

(def unsigned-bytes [(s/pred #(and (> 256 %) (<= 0 %)))])
(def byteorder (s/enum :big-endian :little-endian))

(defn bytes! [data]
  (if (and (seq? data) (every? #(and (number? %) (< % 256)) data)) data nil))

(defn slice [ar start end]
  (let
    [front (first (split-at end ar))]
    (last (split-at start front))))

(def int-size 32)
(def long-size 64)

(defn bits-to-bytes [size]
  (if (and (> size 0) (= (mod size 8) 0)) (/ size 8) nil))

(defn limit-length! [order limit data]
  (let [diff (- limit (count data))
        zero (repeat (short 0))]
    (if (>= diff 0)
      (condp = order
        :little-endian (concat data (take diff zero))
        :big-endian (concat (take diff zero) data))
      nil)))

(defn def-buffer-input
  [convert-fn size]
  (let [size (bits-to-bytes size)]
    (fn [order data]
      (domonad maybe-m
               [data (bytes! data)
                data (limit-length! order size data)]
               (bytes-to-data! convert-fn size order data)))))

(def bytes-to-int (def-buffer-input get-int int-size))
(def bytes-to-long (def-buffer-input get-long long-size))

(defn def-big-integer-input [size]
  (let [size (bits-to-bytes size)]
    (fn [order data]
      (domonad maybe-m
        [size size
         data (bytes! data)
         data (limit-length! order size data)]
        (bytes-to-big-integer! order data)))))

(def bytes-to-i128 (def-big-integer-input 128))

(defn def-unsigned-input
  "java can't support unsigned type! but java support big types. So this implement is using bigger-type and ram-inefficiency, but It work very well!"
  [convert-fn size]
  (let [size (bits-to-bytes size)]
    (fn [order data]
      (convert-fn order (limit-length! order size data)))))

(def bytes-to-unsigned-int (def-unsigned-input bytes-to-long int-size))
(def bytes-to-unsigned-long (def-unsigned-input bytes-to-i128 long-size))

(defn calc-maximum-size [bits]
  (int-pow 2 bits))
(defn calc-minimum-size [bits]
  (int-pow -2 bits))

(defn def-buffer-output [convert-fn size]
  (let [maximum (calc-maximum-size (- size 1))
        minimum (calc-minimum-size (- size 1))
        size (bits-to-bytes size)]
    (fn [order data]
      (domonad maybe-m
               [size size
                order (endian! order)
                :when (and (number? data)
                           (> maximum data)
                           (<= minimum data))]
               (data-to-bytes! convert-fn size order data)))))

(def int-to-bytes (def-buffer-output set-int int-size))
(def long-to-bytes (def-buffer-output set-long long-size))

(defn def-big-integer-output [size]
  (let [maximum (calc-maximum-size (- size 1))
        minimum (calc-minimum-size (- size 1))
        size (bits-to-bytes size)]
    (fn [order data]
      (domonad maybe-m
               [size size
                order (endian! order)
                :when (and (number? data)
                           (> maximum data)
                           (<= minimum data))]
               (limit-length! order size
                              (big-integer-to-bytes! order data))))))

(def i128-to-bytes (def-big-integer-output 128))

(defn cut-length! [order limit data]
  (let [size (count data)
        diff (- size limit)]
    (if (>= diff limit)
      (condp = order
        :little-endian (slice data 0 limit)
        :big-endian (slice data diff size))
      nil)))

(defn def-unsigned-output [convert-fn size]
  (let [size (bits-to-bytes size)]
    (fn [order data]
      (if (<= 0 data)
        (cut-length! order size (convert-fn order data))
        nil))))

(def unsigned-int-to-bytes (def-unsigned-output long-to-bytes int-size))
(def unsigned-long-to-bytes (def-unsigned-output i128-to-bytes long-size))

(defn dynamic-bytes [convert-fn order data]
  (to-big-endian! order
    (drop-while zero?
      (convert-fn :big-endian data))))
