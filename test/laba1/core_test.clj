(ns clusterization.core-test
  (:require [clojure.test :refer :all]
            [clusterization.core :refer :all]))

(deftest euclid-test
  (testing "Wrong result."
    (is (= (euclid [0 3] [1 5]) 2.23606797749979))))


(deftest hamming-test
  (testing "Wrong result."
    (is (= (hamming [0 3] [1 5]) 2))))
	

(deftest consider-potential-test
  (testing "Wrong result."
    (is (= (consider-potentials [[0 3] [1 5]] hamming) '((1.4111122905071873 [0 3]) (1.4111122905071873 [1 5])))
	)
  )
)