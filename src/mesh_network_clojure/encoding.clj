(ns mesh-network-clojure.encoding
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.default :only [BYTEORDER]]
        [mesh-network-clojure.utils :only [slice bytes-to-unsigned-long!]]))

(defstruct rlp :type :data)

(def under-sized-text (short 0x80))
(def over-sized-text (short 0xb8))
(def under-sized-list (short 0xc0))
(def over-sized-list (short 0xf8))

(defn big? [size] (> size 56))

(defn dispense-type! [data-type]
  (letfn [(calc-padding [type size type-pos]
          [type size (- data-type type-pos)])]
    (condp <= data-type
      256 nil
      over-sized-list (calc-padding :List :Over (dec over-sized-list))
      under-sized-list (calc-padding :List :Under under-sized-list)
      over-sized-text (calc-padding :Text :Over (dec over-sized-text))
      under-sized-text (calc-padding :Text :Under under-sized-text)
      (calc-padding :Char :Char data-type))))

(defn calc-position! [[_ size padding]
                      & [^java.nio.ByteOrder order space]]
  (let [pos (+ padding 1)]
    (cond 
      (= size :Under) [1 pos]
      (= size :Char) [0 1]
      (= size :Over) [pos (+ (bytes-to-unsigned-long!
                               order
                               (first (split-at padding space))) pos)])))

(defn deserializer
  ([^java.nio.ByteOrder order data res]
    (lazy-seq
      (if (empty? data)
        res
        (domonad maybe-m
          [type (dispense-type! (first data))
           pos (calc-position! type order (rest data))
           state (cons (first type) pos)
           step (last (split-at (last pos) data))]
          (if (= :List (first type))
            (deserializer order step
              (cons
                (cons state (deserializer order (apply (partial slice data) pos) nil))
                res))
            (deserializer order step (cons state res)))))))
  ([^java.nio.ByteOrder order data] (deserializer order data nil)))

(defn decode-rlp
  "decoding plain text to clojure struct"
  ([^java.nio.ByteOrder order data res] '())
  ([order data] (decode-rlp order data nil))
  ([data] (decode-rlp BYTEORDER data nil)))
