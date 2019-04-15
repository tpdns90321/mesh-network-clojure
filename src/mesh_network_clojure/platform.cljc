(ns mesh-network-clojure.platform)

#?(:clj (import [java.nio ByteOrder]))

(defmulti to-raw! identity)

(defmethod to-raw! :big-endian [_] #?(:clj ByteOrder/BIG_ENDIAN))
(defmethod to-raw! :little-endian [_] #?(:clj ByteOrder/LITTLE_ENDIAN))
