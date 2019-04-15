(ns mesh-network-clojure.platform.bytes-test
  (:require [clojure.test :refer :all]
            [mesh-network-clojure.platform.bytes :as b]))

(deftest bytes-to-data!
  (testing "int little-endian"
    (is (=
         (b/bytes-to-data! b/get-int 4 :little-endian (byte-array (list 255 0 0 0)))
         255)))
  (testing "int big-endian"
    (is (=
         (b/bytes-to-data! b/get-int 4 :big-endian (byte-array (list 0 0 0 255)))
         255)))
  (testing "long little-endian"
    (is (=
         (b/bytes-to-data! b/get-long 8 :little-endian (byte-array (list 255 0 0 0 0 0 0 0)))
         255)))
  (testing "int big-endian"
    (is (=
         (b/bytes-to-data! b/get-long 8 :big-endian (byte-array (list 0 0 0 0 0 0 0 255)))
         255))))

(deftest data-to-bytes!
  (testing "int little-endian"
    (is (=
         (b/data-to-bytes! b/set-int 4 :little-endian 255)
         (list 255 0 0 0))))
  (testing "int big-endian"
    (is (=
         (b/data-to-bytes! b/set-int 4 :big-endian 255)
         (list 0 0 0 255))))
  (testing "long little-endian"
    (is (=
         (b/data-to-bytes! b/set-long 8 :little-endian 255)
         (list 255 0 0 0 0 0 0 0))))
  (testing "long big-endian"
    (is (=
         (b/data-to-bytes! b/set-long 8 :big-endian 255)
         (list 0 0 0 0 0 0 0 255)))))

(deftest bytes-to-big-integer!
  (testing "to little-endian"
    (is (=
          (b/bytes-to-big-integer! :little-endian (byte-array (list 0 1)))
          (biginteger 256))))
  (testing "to big-endian"
    (is (=
          (b/bytes-to-big-integer! :big-endian (byte-array (list 1 0)))
          (biginteger 256)))))

(deftest big-integer-to-bytes!
  (testing "little-endian to"
    (is (=
          (b/big-integer-to-bytes! :little-endian (biginteger 256))
          (list 0 1))))
  (testing "big-endian to"
    (is (=
          (b/big-integer-to-bytes! :big-endian (biginteger 256))
          (list 1 0)))))
