(ns mesh-network-clojure.encoding
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.default :only [BYTEORDER]]
        [mesh-network-clojure.utils :only [slice bytes!]]
        [mesh-network-clojure.utils.bytes :only [bytes-to-unsigned-long
                                                 unsigned-long-to-bytes
                                                 dynamic-bytes]]))

; decoding result struct and input data for encoding to rlp
(defrecord rlp [type data])

; minimumize of type number
(def under-sized-text (short 0x80))
(def over-sized-text (short 0xb8))
(def under-sized-list (short 0xc0))
(def over-sized-list (short 0xf8))

;; use first of rlp bytes for dispense rlp's type
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

;; collect length definition bytes and data space bytes
(defn calc-position! [[_ size padding]  order space]
  (let [pos (+ padding 1)]
    (condp = size
      :Under [1 pos]
      :Char [0 1]
      :Over (domonad maybe-m
                     [res (bytes-to-unsigned-long
                            order
                            (first (split-at padding space)))]
              [pos (+ pos res)]))))

;; extract data definition and position in rlp
(defn deserializer
  ([order counter data]
    (lazy-seq
      (if (empty? data)
        nil
        (domonad maybe-m
          [type (dispense-type! (first data))
           pos (calc-position! type order (rest data))
           state (cons (first type) (map #(+ counter %) pos))
           step (deserializer order (last state) (last (split-at (last pos) data)))]
          (if (= :List (first type))
            (cons
              (cons state
                (deserializer order
                  (second state) (apply (partial slice data) pos))) step)
            (cons state step))))))
  ([order data] (deserializer order 0 data)))

(defn decode-rlp
  "decoding plain text to clojure struct"
  ([order data states]
    (map
      #(if (keyword? (first %))
        (domonad maybe-m
                 [[type & pos] %
                  inner (bytes! (apply (partial slice data) pos))]
          (rlp. type inner))
        (rlp. :List (decode-rlp order data (rest %)))) states))
  ([order data]
    (decode-rlp order data (deserializer BYTEORDER data)))
  ([data] (decode-rlp BYTEORDER data)))

(defn big? [size] (> size 55))

(defn generate-padding [under over order length]
  (if (big? length)
    (let [padding (dynamic-bytes unsigned-long-to-bytes order length)
          type-pos (+ over (dec (count padding)))]
      (cons type-pos padding))
    (list (+ under length))))

(def list-padding (partial generate-padding under-sized-list over-sized-list))
(def text-padding (partial generate-padding under-sized-text over-sized-text))

(defn encode-rlp
  ([order rlps res tmp]
   (if (empty? rlps)
     (if (nil? tmp)
       res
       (recur order (first tmp)
              (concat (second tmp) (list-padding order (count res)) res)
              (nth tmp 2)))
     (let [{type :type data :data} (first rlps)]
       (if (= type :List)
         (recur order data nil (list (rest rlps) res tmp))
         (recur order
             (rest rlps)
             (concat
               (condp = type
                 :Char data
                 :Text (concat
                         (text-padding order (count data))
                         data))
               res) tmp)))))
  ([order rlps]
   (encode-rlp order (if (seq? rlps) rlps (list rlps)) nil nil)))
