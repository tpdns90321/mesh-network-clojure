(ns mesh-network-clojure.utils.bytes-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [mesh-network-clojure.utils.bytes :as b]))

(defmacro check-type-error [func]
  `(is (try (s/with-fn-validation ~func)
       (catch clojure.lang.ExceptionInfo e# true))))

(deftest bytes-to-int
  (testing "check length limit"
    (is (nil? (b/bytes-to-int :big-endian (list 1 0 0 0 0)))))
  (testing "check range of unsigned byte"
    (check-type-error (b/bytes-to-int :big-endian (list 425 0 0 0))))
  (testing "not seq"
    (check-type-error (b/bytes-to-int :big-endian :blabla)))
  (testing "not byte element"
    (check-type-error (b/bytes-to-int :big-endian (list :blabla 0 0 0)))))

(deftest bytes-to-big-i128
  (testing "check length limit"
    (is (nil?
          (b/bytes-to-i128
            :big-endian (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10)))))
  (testing "check range of unsigned bytes"
    (check-type-error
          (b/bytes-to-i128 :big-endian (list 429 0 0))))
  (testing "not seq"
    (check-type-error (b/bytes-to-i128 :big-endian :blabla)))
  (testing "not byte element"
    (check-type-error (b/bytes-to-i128 :big-endian (list :blabla 0 0 0)))))

(deftest int-to-bytes
  (testing "check maximum size"
    (check-type-error (b/int-to-bytes :big-endian (+ (b/calc-maximum-size 31) 1))))
  (testing "check minimum size"
    (check-type-error (b/int-to-bytes :big-endian (- (b/calc-minimum-size 31) 1))))
  (testing "check number"
    (check-type-error (b/int-to-bytes :big-endian :test))))

(deftest i128-to-bytes
  (testing "check maximum size"
    (check-type-error (b/i128-to-bytes :big-endian (+ (b/calc-maximum-size 127) 1))))
  (testing "check minimum size"
    (check-type-error (b/i128-to-bytes :big-endian (- (b/calc-minimum-size 127) 1))))
  (testing "check number"
    (check-type-error (b/i128-to-bytes :big-endian :test))))
