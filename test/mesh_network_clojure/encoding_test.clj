(ns mesh-network-clojure.encoding-test
  (:require [clojure.test :refer :all]
            [mesh-network-clojure.encoding :as encoding]
            [mesh-network-clojure.default :as d]))

(defn text-n-bytes [n]
  (let [n (- n 1)]
    (if (encoding/big? n)
      (apply (partial conj [encoding/over-sized-text (- n 1)]) (repeat (- n 1) 0))
      (apply (partial conj [(+ encoding/under-sized-text n)]) (repeat n 0)))))

(deftest deserializer
  "extract encoding element status"
  (testing "text"
    (is (=
         (encoding/deserializer d/BYTEORDER (text-n-bytes 10))
         (list '(:Text 1 10)))))
  (testing "list"
    (is (=
         (encoding/deserializer d/BYTEORDER
                                (concat
                                  [(+ encoding/under-sized-list 10)]
                                  (text-n-bytes 10)))
         (list (list '(:List 1 11) '(:Text 2 11))))))
  (testing "recursion list"
    (is (=
         (encoding/deserializer d/BYTEORDER (map #(+ encoding/under-sized-list %) [2 1 0]))
         (list (list '(:List 1 3) (list '(:List 2 3) (list '(:List 3 3))))))))
  (testing "long text"
    (is (=
         (encoding/deserializer d/BYTEORDER (text-n-bytes 100))
         (list '(:Text 2 100)))))
  (testing "long list"
    (is (=
         (encoding/deserializer d/BYTEORDER
                                (concat (list encoding/over-sized-list 100) (text-n-bytes 100)))
         (list '((:List 2 102) (:Text 4 102)))))))
