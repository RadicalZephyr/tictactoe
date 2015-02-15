(ns tictactoe.board-test
  (:require [tictactoe.board :refer :all]
            [clojure.test :refer :all]
            [clojure.math.combinatorics :refer [subsets]]))

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

(deftest game-result-test
  (are [result board marks]
    (= (game-result board marks)
       result)

    nil
    [" " " " " "
     " " " " " "
     " " " " " "]
    {:player "x"
     :ai     "o"}

    :player
    [" " " " "x"
     " " "x" " "
     "x" " " " "]
    {:player "x"
     :ai     "o"}

    :ai
    [" " " " "o"
     " " "o" " "
     "o" " " " "]
    {:player "x"
     :ai     "o"}

    :draw
    ["o" "x" "o"
     "x" "o" "x"
     "x" "o" "x"]
    {:player "x"
     :ai     "o"}

    :draw
    ["o" "x" "o"
     "o" "x" "x"
     "x" "o" "x"]
    {:player "x"
     :ai     "o"}

    :draw
    ["o" "x" "o"
     "x" "x" "o"
     "o" "o" "x"]
    {:player "x"
     :ai     "o"}))

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
      false [0 1] [0 1]))

  (testing "All symmetries"
    (are [result board]
      (= (get-symmetries board)
         result)

      #{:lr :tb :tl-br :tr-bl} empty-board

      #{} (range 9)

      #{} ["x" "x" " "
           " " "x" " "
           " " " " " "]

      #{:lr} ["x" " " "x"
              " " " " " "
              " " " " " "]

      #{:tb} ["x" " " " "
              " " " " " "
              "x" " " " "]

      #{:tl-br} ["o" " " "x"
                 " " " " " "
                 "x" " " " "]

      #{:tr-bl} ["x" " " " "
                 " " " " " "
                 "o" " " "x"]

      #{:tl-br :tr-bl} [" " " " "x"
                        " " " " " "
                        "x" " " " "]

      #{:tl-br :tr-bl} ["x" " " " "
                        " " " " " "
                        " " " " "x"])))

(deftest move-equivalences
  (testing "Move equivalences"
    (are [result symmetries index]
      (= (get-all-equivalent-moves symmetries index)
         result)

      #{0 2 6} #{:lr :tb} 0
      #{0 2 8} #{:lr :tb} 2
      #{0 6 8} #{:lr :tb} 6
      #{2 6 8} #{:lr :tb} 8)

    (doseq [index [0 2 6 8]]
      (is (= (get-all-equivalent-moves #{:lr :tb :tl-br :tr-bl} index)
             #{0 2 6 8})))

    ;; If there is no symmetry, all tiles are unique
    (doseq [index (range 9)]
      (is (= (get-all-equivalent-moves #{} index)
             #{index})))

    ;; No matter what the symmetry, nothing is ever equivalent with
    ;; the center tile
    (doseq [symm (subsets [:lr :tb :tl-br :tr-bl])]
      (is (= (get-all-equivalent-moves (set symm) 4)
             #{4}))))

  (testing "Get unique moves of a board"
    (are [result board]
      (= (get-unique-move-sets board)
         result)

      #{#{4}
        #{0 2 6 8}
        #{1 3 5 7}}
      empty-board

      #{#{4}
        #{8}
        #{1 3}
        #{5 7}
        #{2 6}}
      ["x" " " " "
       " " " " " "
       " " " " " "]

      #{#{4}
        #{7}
        #{0 2}
        #{3 5}
        #{6 8}}
      [" " "x" " "
       " " " " " "
       " " " " " "]

      #{#{0 2 6 8}
        #{1 3 5 7}}
      [" " " " " "
       " " "x" " "
       " " " " " "]

      #{#{4}
        #{1 3 5}
        #{1 3 7}
        #{1 5 7}
        #{3 5 7}
        #{2 6}}
      ["x" " " " "
       " " " " " "
       " " " " "x"])))
