(ns tictactoe.board-test
  (:require [tictactoe.board :refer :all]
            [clojure.test :refer :all]))

(deftest which-winner?-test
  (testing "No false positives"
    (is (= (which-winner? [" " " " " "
                           " " " " " "
                           " " " " " "])
           nil) "Empty board should have no winner.")

    (is (= (which-winner? ["x" "x" "o"
                           "o" "x" "x"
                           "x" "o" "o"])
           nil) "Cat's game should have no winner.")

    (is (= (which-winner? ["x" "x" " "
                           " " "x" "o"
                           " " "o" "o"])
           nil) "Unfinished game should have no winner."))

  (doseq [mark ["x" "o" "b"]]
    (testing "All winning positions"
      (is (= (which-winner? [mark mark mark
                             " " " " " "
                             " " " " " "])
             mark))
      (is (= (which-winner? [" " " " " "
                             mark mark mark
                             " " " " " "])
             mark))
      (is (= (which-winner? [" " " " " "
                             " " " " " "
                             mark mark mark])
             mark))
      (is (= (which-winner? [mark " " " "
                             mark " " " "
                             mark " " " "])
             mark))
      (is (= (which-winner? [" " mark " "
                             " " mark " "
                             " " mark " "])
             mark))
      (is (= (which-winner? [" " " " mark
                             " " " " mark
                             " " " " mark])
             mark))
      (is (= (which-winner? [mark " " " "
                             " " mark " "
                             " " " " mark])
             mark))
      (is (= (which-winner? [" " " " mark
                             " " mark " "
                             mark " " " "])
             mark)))))

(deftest indexed-groups-test
  (testing "Output"
    (is (= (sort
            (indexed-board [1 2 3 4 5 6 7 8 9]))
           (sort
            [[[1 1] 1] [[2 1] 2] [[3 1] 3]
             [[1 2] 4] [[2 2] 5] [[3 2] 6]
             [[1 3] 7] [[2 3] 8] [[3 3] 9]])))))
