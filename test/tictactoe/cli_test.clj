(ns tictactoe.cli-test
  (:require [tictactoe.cli :refer :all]
            [clojure.test :refer :all]))

(deftest label-board-test
  (testing ""
    (is (= (label-board [0 0 0
                         0 0 0
                         0 0 0])
           '((0 0 0 1)
             (0 0 0 2)
             (0 0 0 3))))
    (is (= (label-board [0 0 1
                         0 2 0
                         3 0 0])
           '((0 0 1 1)
             (0 2 0 2)
             (3 0 0 3))))
    (is (= (label-board [1 0 0
                         0 2 0
                         0 0 3])
           '((1 0 0 1)
             (0 2 0 2)
             (0 0 3 3))))))

(deftest num->xy-test
  (testing "Regular numbers"
    (is (= (num->xy 11)
           [1 1]))
    (is (= (num->xy 13)
           [1 3]))
    (is (= (num->xy 71)
           [7 1]))
    (is (= (num->xy 22)
           [2 2]))))

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
