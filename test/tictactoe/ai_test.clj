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
        "Two spaces and a mark are not a winning move."))

  (testing "True positives"
    (is (= (check-winning-move '(" " "x" "x"))
           "x"))
    (is (= (check-winning-move '("x" " " "x"))
           "x"))
    (is (= (check-winning-move '("x" "x" " "))
           "x")))

  (testing "True negatives"
    (is (and
         (= (check-winning-move '("o" "o" "x"))
            nil)
         (= (check-winning-move '("x" "x" "o"))
            nil)
         (= (check-winning-move '("x" "o" "o"))
            nil)
         (= (check-winning-move '("o" "x" "x"))
            nil)
         (= (check-winning-move '("x" "o" "x"))
            nil)
         (= (check-winning-move '("o" "x" "o"))
            nil))
        "No spaces mean it's not a winning move.")))

(deftest is-winning-group?-test
  (testing "False positives"
    (doseq [mark ["x" "o"]
            op-mark ["o" "x"]]
      (is (= (winning-group [[[0 0] mark] [[0 1] op-mark] [[0 2] op-mark]])
             nil))
      (is (= (winning-group [[[0 0] op-mark] [[0 1] mark] [[0 2] op-mark]])
             nil))
      (is (= (winning-group [[[0 0] op-mark] [[0 1] op-mark] [[0 2] mark]])
             nil))))

  (testing "True positives"
    (doseq [mark ["x" "o"]]
      (is (= (winning-group [[[0 0] " "] [[0 1] mark] [[0 2] mark]])
            [[0 0] mark]))
      (is (= (winning-group [[[0 0] mark] [[0 1] " "] [[0 2] mark]])
            [[0 1] mark]))
      (is (= (winning-group [[[0 0] mark] [[0 1] mark] [[0 2] " "]])
             [[0 2] mark])))))

(deftest get-winning-move-test
  (testing "True negatives"
    (is (= (get-winning-move [" " " " " "
                              " " " " " "
                              " " " " " "]
                             "x")
           nil))
    (is (= (get-winning-move ["o" "x" "o"
                              "o" "x" "o"
                              "x" "o" "x"]
                             "x")
           nil))
    (is (= (get-winning-move ["o" "x" " "
                              "x" " " "o"
                              " " "o" "x"]
                             "x")
           nil)))

  (testing "Simple positives"
    (doseq [mark ["x" "o"]
            op-mark ["o" "x"]]
      (is (= (get-winning-move [" " mark mark
                                " " " " " "
                                " " " " " "]
                               op-mark)
             [1 1]))
      (is (= (get-winning-move [mark mark " "
                                " " " " " "
                                " " " " " "]
                               op-mark)
             [3 1]))

      (is (= (get-winning-move [" " " " " "
                                mark " " " "
                                mark " " " "]
                               op-mark)
             [1 1]))
      (is (= (get-winning-move [mark " " " "
                                mark " " " "
                                " " " " " "]
                               op-mark)
             [1 3]))

      (is (= (get-winning-move [" " " " " "
                                " " mark " "
                                " " " " mark]
                               op-mark)
             [1 1]))
      (is (= (get-winning-move [mark " " " "
                                " " mark " "
                                " " " " " "]
                               op-mark)
             [3 3]))

      (is (= (get-winning-move [" " " " " "
                                " " mark " "
                                mark " " " "]
                               op-mark)
             [3 1]))
      (is (= (get-winning-move [" " " " mark
                                " " mark " "
                                " " " " " "]
                               op-mark)
             [1 3])))))

(def pos-inf Double/POSITIVE_INFINITY)
(def neg-inf Double/NEGATIVE_INFINITY)

(deftest minimax-test
  (testing "Completed boards"
    (is (= (minimax ["o" "x" "o"
                     "o" "x" "x"
                     "o" "o" "x"]
                    :ai {:ai "o" :player "x"})
           pos-inf))

    (is (= (minimax ["o" "x" "o"
                     "o" "x" "x"
                     "x" "x" "o"]
                    :ai {:ai "o" :player "x"})
           neg-inf)))

  (testing "One to complete boards"
    (is (= (minimax ["o" "x" "o"
                     "o" "x" "x"
                     "x" " " "o"]
                    :player {:ai "o" :player "x"})
           neg-inf))

    (is (= (minimax ["o" "x" "o"
                     "o" "x" "x"
                     " " "o" "x"]
                    :ai {:ai "o" :player "x"})
           pos-inf))

    (is (= (minimax [" " " " " "
                     "o" "x" " "
                     "x" " " " "]
                    :player {:ai "o" :player "x"})
           neg-inf))

    (is (= (minimax [" " " " " "
                     "o" "x" " "
                     "x" " " " "]
                    :ai {:ai "x" :player "o"})
           pos-inf)))

  (testing "Fork handling"
    (is (= (minimax ["x" " " "x"
                     " " "o" " "
                     " " " " "x"]
                    :ai {:ai "o" :player "x"})
           neg-inf))

    (is (= (minimax ["o" "o" "x"
                     " " "x" " "
                     " " " " "x"]
                    :ai {:ai "o" :player "x"})
           neg-inf)))

  (testing "Fork prediction"
    (is (= (minimax ["x" " " "o"
                     " " "o" " "
                     " " " " "x"]
                    :player {:ai "o" :player "x"})
           neg-inf))

    (is (= (minimax ["x" " " "o"
                     " " "o" " "
                     " " " " "x"]
                    :ai {:ai "x" :player "o"})
           pos-inf))

    (is (= (minimax [" " " " " "
                     " " "x" "o"
                     " " " " " "]
                    :player {:ai "o" :player "x"})
           neg-inf))

    (is (= (minimax [" " " " " "
                     " " "x" "o"
                     " " " " " "]
                    :ai {:ai "x" :player "o"})
           pos-inf)))

  (testing "Previous AI failure modes"
    (is (= (minimax ["x" "o" " "
                     " " "o" " "
                     " " " " "x"]
                    :player {:ai "o" :player "x"})
           0))
    (is (= (minimax ["x" " " " "
                     " " "o" " "
                     " " " " "x"]
                    :ai {:ai "o" :player "x"})
           0))))
