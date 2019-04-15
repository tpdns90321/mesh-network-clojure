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
