(ns mesh-network-clojure.encoding.rlp
  (:require [schema.core :as s])
  (:use [clojure.algo.monads :only [domonad maybe-m]]
        [mesh-network-clojure.default :only [BYTEORDER]]
        [mesh-network-clojure.utils.bytes :only [unsigned-bytes
                                                 byteorder
                                                 bytes-to-unsigned-long
                                                 unsigned-long-to-bytes
                                                 dynamic-bytes
                                                 slice bytes!]]))

(def rlp-type (s/enum :List :Text :Char :Error))
; decoding result struct and input data for encoding to rlp
; (defrecord rlp [type data])
(def rlp (s/conditional
           #(= (:type %) :List) {:type (s/eq :List) :data [(s/recursive #'rlp)]}
           #(s/validate rlp-type (:type %)) {:type rlp-type :data unsigned-bytes}))

(s/defn create-rlp :- rlp [type :- rlp-type
                           data :- [s/Any]] {:type type, :data data})

; minimumize of type number
(def under-sized-text (short 0x80))
(def over-sized-text (short 0xb8))
(def under-sized-list (short 0xc0))
(def over-sized-list (short 0xf8))

(def rlp-size (s/enum :Over :Under :Char))
(def dispense-type-res (s/maybe [(s/one rlp-type "type")
                                 (s/one rlp-size "size")
                                 (s/one s/Num "padding")]))

;; use first of rlp bytes for dispense rlp's type
(s/defn dispense-type! :- dispense-type-res
  [data-type :- s/Num]
  (letfn [(calc-padding [type size type-pos]
          [type size (- data-type type-pos)])]
    (condp <= data-type
      256 nil
      over-sized-list (calc-padding :List :Over (dec over-sized-list))
      under-sized-list (calc-padding :List :Under under-sized-list)
      over-sized-text (calc-padding :Text :Over (dec over-sized-text))
      under-sized-text (calc-padding :Text :Under under-sized-text)
      0 (calc-padding :Char :Char data-type)
      nil)))

;; collect length definition bytes and data space bytes
(s/defn calc-position! :- (s/maybe [(s/one s/Num "start")
                                    (s/one s/Num "end")])
  [[_ size padding] :- dispense-type-res
   order :- byteorder space :- unsigned-bytes]
  (let [pos (+ padding 1)]
    (condp = size
      :Under [1 pos]
      :Char [0 1]
      :Over (domonad maybe-m
                     [res (bytes-to-unsigned-long
                            order
                            (first (split-at padding space)))]
              [pos (+ pos res)]))))

(def deserializer-res [[(s/one rlp-type "type")
                        (s/one s/Num "start")
                        (s/one s/Num "end")
                        (s/optional (s/recursive #'deserializer-res) "list content")]])

;; extract data definition and position in rlp
(s/defn deserializer :- deserializer-res
  ([order :- byteorder counter :- s/Num data :- unsigned-bytes]
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
              (concat state
                      (list
                        (deserializer order
                          (second state) (apply (partial slice data) pos)))) step)
            (cons state step))))))
  ([order data] (deserializer order 0 data)))

(s/defn decode-rlp :- [rlp]
  "decoding plain text to clojure struct"
  ([order :- byteorder
    data :- unsigned-bytes
    states :- deserializer-res]
    (map
      #(if (= (first %) :List)
        (create-rlp :List (decode-rlp order data (last %)))
        (let [[type & pos] %
              inner (apply (partial slice data) pos)]
          (create-rlp type inner))) states))
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
   (loop [rlps rlps
          res res
          tmp tmp]
     (if (empty? rlps)
       (if (nil? tmp)
         res
         (recur (first tmp)
                (concat (second tmp) (list-padding order (count res)) res)
                (nth tmp 2)))
       (let [{type :type data :data} (first rlps)]
         (if (= type :List)
           (recur data nil (list (rest rlps) res tmp))
           (recur (rest rlps)
                    (concat
                      (condp = type
                        :Char data
                        :Text (concat
                                (text-padding order (count data))
                                data))
                      res) tmp))))))
  ([order rlps]
   (encode-rlp order (if (seq? rlps) rlps (list rlps)) nil nil)))
