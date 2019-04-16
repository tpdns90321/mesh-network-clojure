(ns mesh-network-clojure.utils.bytes-test
  (:require [clojure.test :refer :all]
            [mesh-network-clojure.utils.bytes :as b]))

(deftest bytes-to-int
  (testing "check length limit"
    (is (nil? (b/bytes-to-int :big-endian (list 1 0 0 0 0)))))
  (testing "check range of unsigned byte"
    (is (nil? (b/bytes-to-int :big-endian (list 425 0 0 0))))))
