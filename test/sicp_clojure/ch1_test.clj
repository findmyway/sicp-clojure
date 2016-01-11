(ns sicp-clojure.ch1-test
  (:require [clojure.test :refer :all]
            [sicp-clojure.ch1 :refer :all]))

(deftest ch1-3-test
  (testing "sum-square-larger2"
    (is (= (sum-square-larger2 1 2 3) 13))
    (is (= (sum-square-larger2 -1 -2 -3) 5))
    ))

(deftest ch1-4-test
  (testing "a-plus-abs-b"
    (is (= (a-plus-abs-b 1 2) 3))
    (is (= (a-plus-abs-b 1 -2) 3))
    ))

(deftest ch1-5-test
  (testing "judge interpreter eval order"
    (is (thrown? StackOverflowError (judge-interpreter 0 (p))))))
