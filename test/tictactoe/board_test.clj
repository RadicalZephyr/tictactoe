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
            [(make-indexed-cell [1 1] 1) (make-indexed-cell [2 1] 2) (make-indexed-cell [3 1] 3)
             (make-indexed-cell [1 2] 4) (make-indexed-cell [2 2] 5) (make-indexed-cell [3 2] 6)
             (make-indexed-cell [1 3] 7) (make-indexed-cell [2 3] 8) (make-indexed-cell [3 3] 9)])))))

(deftest symmetries-test
  (testing "Individual symmetry fns"
    (are [board]
      (= (has-lr-sym? board)
         :lr)

      [" " " " " "
       " " " " " "
       " " " " " "]

      [" " " " " "
       "x" " " "x"
       " " " " " "]

      ["x" " " "x"
       "o" " " "o"
       "x" " " "x"]

      ["x" " " "x"
       " " " " " "
       "o" " " "o"])

    (are [board]
      (= (has-tb-sym? board)
         :tb)

      [" " " " " "
       " " " " " "
       " " " " " "]

      [" " "x" " "
       " " " " " "
       " " "x" " "]

      ["x" "o" "x"
       " " " " " "
       "x" "o" "x"]

      ["x" " " "o"
       " " " " " "
       "x" " " "o"])

    (are [board]
      (= (has-tl-br-sym? board)
         :tl-br)

      [" " " " " "
       " " " " " "
       " " " " " "]

      [" " " " "x"
       " " " " " "
       "x" " " " "]

      [" " "x" "o"
       "x" " " "x"
       "o" "x" " "]

      [" " "x" " "
       "x" " " "o"
       " " "o" " "])

    (are [board]
      (= (has-tr-bl-sym? board)
         :tr-bl)

      [" " " " " "
       " " " " " "
       " " " " " "]

      ["x" " " " "
       " " " " " "
       " " " " "x"]

      ["o" "x" " "
       "x" " " "x"
       " " "x" "o"]

      [" " "x" " "
       "o" " " "x"
       " " "o" " "])

    (are [result inds board]
      (= (=at-index board inds)
         result)

      true  [0 1] [0 0]
      true  [1 3] [0 1 3 1]
      false [0 1] [0 1])))
