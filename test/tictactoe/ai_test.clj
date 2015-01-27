(ns tictactoe.ai-test
  (:require [tictactoe.ai :refer :all]
            [clojure.test :refer :all]))

(deftest winning-move-test
  (testing "False positives"
    (is (and
         (= (check-winning-move '(" " "x" " "))
            nil)
         (= (check-winning-move '(" " "o" " "))
            nil))
        "Two spaces and a mark are not a winning move.")

    (is (and
         (= (check-winning-move '("o" "o" "x"))
            nil)
         (= (check-winning-move '("x" "x" "o"))
            nil)
         (= (check-winning-move '("x" "o" "o"))
            nil)
         (= (check-winning-move '("o" "x" "x"))
            nil)
         (= (check-winning-move '("o" "x" "x"))
            nil))
        "No spaces mean it's not a winning move."))

  (testing "True positives"
    (is (= (check-winning-move '(" " "x" "x"))
           "x"))
    (is (= (check-winning-move '("x" " " "x"))
           "x"))
    (is (= (check-winning-move '("x" "x" " "))
           "x")))

  (testing "True negatives"
    (is (= (check-winning-move '("o" "x" "x"))
           nil))
    (is (= (check-winning-move '("x" "o" "x"))
           nil))
    (is (= (check-winning-move '("x" "x" "o"))
           nil))
    (is (= (check-winning-move '("x" "o" "o"))
           nil))
    (is (= (check-winning-move '("o" "x" "o"))
           nil))
    (is (= (check-winning-move '("o" "o" "x"))
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
