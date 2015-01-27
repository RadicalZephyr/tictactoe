(ns tictactoe.board-test
  (:require [tictactoe.board :refer :all]
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
           nil) "Unfinished game should have no winner."))

  (doseq [mark ["x" "o" "b"]]
   (testing "All winning positions"
     (is (= (winner? [mark mark mark
                      " " " " " "
                      " " " " " "])
            mark))
     (is (= (winner? [" " " " " "
                      mark mark mark
                      " " " " " "])
            mark))
     (is (= (winner? [" " " " " "
                      " " " " " "
                      mark mark mark])
            mark))
     (is (= (winner? [mark " " " "
                      mark " " " "
                      mark " " " "])
            mark))
     (is (= (winner? [" " mark " "
                      " " mark " "
                      " " mark " "])
            mark))
     (is (= (winner? [" " " " mark
                      " " " " mark
                      " " " " mark])
            mark))
     (is (= (winner? [mark " " " "
                      " " mark " "
                      " " " " mark])
            mark))
     (is (= (winner? [" " " " mark
                      " " mark " "
                      mark " " " "])
            mark)))))

(deftest indexed-groups-test
  (testing "Output"
    (is (= (indexed-board [1 2 3 4 5 6 7 8 9])
           [[[0 0] 1] [[0 1] 2] [[0 2] 3]
            [[1 0] 4] [[1 1] 5] [[1 2] 6]
            [[2 0] 7] [[2 1] 8] [[2 2] 9]]))))
