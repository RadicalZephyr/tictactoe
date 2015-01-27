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
