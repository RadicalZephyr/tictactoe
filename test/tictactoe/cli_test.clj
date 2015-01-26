(ns tictactoe.cli-test
  (:require [tictactoe.cli :refer :all]
            [clojure.test :refer :all]))

(deftest winner?-test
  (testing "No false positives"
    (is (= (winner? [" " " " " "
                     " " " " " "
                     " " " " " "])
           nil) "Empty board should have no winner.")
    (is (= (winner? ["x" "x" "o"
                     "o" "x" "x"
                     "x" "o" "o"])
           nil) "Cat's game should have no winner.")
    (is (= (winner? ["x" "x" " "
                     " " "x" "o"
                     " " "o" "o"])
           nil) "Unfinished game should have no winner.")))
