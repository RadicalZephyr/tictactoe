(ns tictactoe.ai-test
  (:require [tictactoe.ai :refer :all]
            [clojure.test :refer :all]))

(deftest winning-move-test
  (testing "False positives"
    (is (and
         (= (winning-move '(" " "x" " "))
            nil)
         (= (winning-move '(" " "o" " "))
            nil))
        "Two spaces and a mark are not a winning move.")

    (is (and
         (= (winning-move '("o" "o" "x"))
            nil)
         (= (winning-move '("x" "x" "o"))
            nil)
         (= (winning-move '("x" "o" "o"))
            nil)
         (= (winning-move '("o" "x" "x"))
            nil)
         (= (winning-move '("o" "x" "x"))
            nil))
        "No spaces mean it's not a winning move."))

  (testing "True positives"
    (is (= (winning-move '(" " "x" "x"))
           "x"))
    (is (= (winning-move '("x" " " "x"))
           "x"))
    (is (= (winning-move '("x" "x" " "))
           "x")))

  (testing "True negatives"
    (is (= (winning-move '("o" "x" "x"))
           nil))
    (is (= (winning-move '("x" "o" "x"))
           nil))
    (is (= (winning-move '("x" "x" "o"))
           nil))
    (is (= (winning-move '("x" "o" "o"))
           nil))
    (is (= (winning-move '("o" "x" "o"))
           nil))
    (is (= (winning-move '("o" "o" "x"))
           nil))))

(deftest is-winning-group?-test
  (testing "False positives"
    (is (= (winning-group '([[0 0] "x"] [[0 1] "o"] [[0 2] "o"]))
           nil))
    (is (= (winning-group '([[0 0] "o"] [[0 1] "x"] [[0 2] "o"]))
           nil))
    (is (= (winning-group '([[0 0] "o"] [[0 1] "o"] [[0 2] "x"]))
           nil)))

  (testing "True positives"
    (is (= (winning-group '([[0 0] " "] [[0 1] "o"] [[0 2] "o"]))
           [[0 0] "o"]))
    (is (= (winning-group '([[0 0] "o"] [[0 1] " "] [[0 2] "o"]))
           [[0 1] "o"]))
    (is (= (winning-group '([[0 0] "o"] [[0 1] "o"] [[0 2] " "]))
           [[0 2] "o"]))))