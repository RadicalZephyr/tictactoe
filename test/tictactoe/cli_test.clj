(ns tictactoe.cli-test
  (:require [tictactoe.cli :refer :all]
            [clojure.test :refer :all]))

(deftest is-move-collection?-test
  (testing "Positive's"
    (is (= (is-move-collection? [1 2])
           true))
    (is (= (is-move-collection? '(2 3))
           true)))
  (testing "Negative's"
    (is (= (is-move-collection? {:x 1 :y 2})
           false))
    (is (= (is-move-collection? #{1 2})
           false))
    (is (= (is-move-collection? [4 1])
           false))
    (is (= (is-move-collection? [1 4])
           false))
    (is (= (is-move-collection? [4 0])
           false))))
