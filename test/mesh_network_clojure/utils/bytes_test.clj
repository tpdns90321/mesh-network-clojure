(ns mesh-network-clojure.utils.bytes-test
  (:require [clojure.test :refer :all]
            [mesh-network-clojure.utils.bytes :as b]))

(deftest bytes-to-int
  (testing "check length limit"
    (is (nil? (b/bytes-to-int :big-endian (list 1 0 0 0 0)))))
  (testing "check range of unsigned byte"
    (is (nil? (b/bytes-to-int :big-endian (list 425 0 0 0)))))
  (testing "not seq"
    (is (nil? (b/bytes-to-int :big-endian :blabla))))
  (testing "not byte element"
    (is (nil? (b/bytes-to-int :big-endian (list :blabla 0 0 0))))))

(deftest bytes-to-big-i128
  (testing "check length limit"
    (is
      (nil? (b/bytes-to-i128
              :big-endian (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10)))))
  (testing "check range of unsigned bytes"
    (is
        (nil?
          (b/bytes-to-i128 :big-endian (list 429 0 0)))))
  (testing "not seq"
    (is (nil? (b/bytes-to-i128 :big-endian :blabla))))
  (testing "not byte element"
    (is (nil? (b/bytes-to-i128 :big-endian (list :blabla 0 0 0))))))

(deftest int-to-bytes
  (testing "check maximum size"
    (is (nil? (b/int-to-bytes :big-endian (+ (b/calc-maximum-size 31) 1)))))
  (testing "check minimum size"
    (is (nil? (b/int-to-bytes :big-endian (- (b/calc-minimum-size 31) 1)))))
  (testing "check number"
    (is (nil? (b/int-to-bytes :big-endian :test)))))

(deftest i128-to-bytes
  (testing "check maximum size"
    (is (nil? (b/i128-to-bytes :big-endian (+ (b/calc-maximum-size 127) 1)))))
  (testing "check minimum size"
    (is (nil? (b/i128-to-bytes :big-endian (- (b/calc-minimum-size 127) 1)))))
  (testing "check number"
    (is (nil? (b/i128-to-bytes :big-endian :test)))))
