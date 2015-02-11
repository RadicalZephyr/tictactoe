(ns tictactoe.ai-test
  (:require [tictactoe.ai :refer :all]
            [clojure.test :refer :all]
            [tictactoe.board :as board]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))


(def char-strings (gen/fmap str gen/char-alphanumeric))

(def two-chars (gen/tuple char-strings
                          char-strings))

(def true-negative-attack-gen (gen/fmap (fn [[a b]]
                                          (shuffle [a a b]))
                                        two-chars))

(def false-positive-attack-gen (gen/fmap (fn [x]
                                           (shuffle [x
                                                     board/blank
                                                     board/blank]))
                                         char-strings))

(def true-positive-attack-gen (gen/fmap (fn [x]
                                          [x (shuffle [x x board/blank])])
                                        char-strings))

(defspec true-negatives-are-not-wins
  (prop/for-all [attack true-negative-attack-gen]
    (= (check-winning-move attack)
       nil)))

(defspec false-positives-are-not-wins
  (prop/for-all [attack false-positive-attack-gen]
    (= (check-winning-move attack)
       nil)))

(defspec true-positives-win-with-double-mark
  (prop/for-all [[val attack] true-positive-attack-gen]
    (= (check-winning-move attack)
       val)))

(deftest winning-move-test
  (testing "False positives"
    (is (= (check-winning-move '("x" " " " "))
           nil))
    (is (= (check-winning-move '(" " "x" " "))
           nil))
    (is (= (check-winning-move '(" " " " "x"))
           nil))
    "Two spaces and a mark are not a winning move.")

  (testing "True positives"
    (is (= (check-winning-move '(" " "x" "x"))
           "x"))
    (is (= (check-winning-move '("x" " " "x"))
           "x"))
    (is (= (check-winning-move '("x" "x" " "))
           "x")))

  (testing "True negatives"
    (is (= (check-winning-move '("o" "o" "x"))
           nil))
    (is (= (check-winning-move '("x" "x" "o"))
           nil))
    (is (= (check-winning-move '("x" "o" "o"))
           nil))
    (is (= (check-winning-move '("o" "x" "x"))
           nil))
    (is (= (check-winning-move '("x" "o" "x"))
           nil))
    (is (= (check-winning-move '("o" "x" "o"))
           nil))
    "No spaces mean it's not a winning move."))

(defn simple-indexer [attack]
  (vec (map vector
            (map (fn [x]
                   [0 x])
                 (range 3))
            attack)))

(defn compound-indexer [[el attack]]
  (let [indexed-attack (simple-indexer attack)]
    [(some (fn [[idx mark]]
             (when (= mark board/blank)
               [idx el]))
           indexed-attack)
     indexed-attack]))

(defn indexed-attacks [attack-generator indexer]
  (gen/fmap indexer attack-generator))

(defspec true-negatives-are-not-winning-attacks
  (prop/for-all [attack (indexed-attacks true-negative-attack-gen
                                         simple-indexer)]
    (= (winning-attack? attack)
       nil)))

(defspec false-positives-are-not-winning-attacks
  (prop/for-all [attack (indexed-attacks false-positive-attack-gen
                                         simple-indexer)]
    (= (winning-attack? attack)
       nil)))

(defspec true-positives-are-winning-attacks
  (prop/for-all [[el attack] (indexed-attacks true-positive-attack-gen
                                              compound-indexer)]
    (= (winning-attack? attack)
       el)))

(deftest winning-attack?-test
  (testing "False positives"
    (doseq [mark ["x" "o"]
            op-mark ["o" "x"]]
      (are [attack]
        (= (winning-attack? attack)
           nil)

        [[[0 0] mark] [[0 1] op-mark] [[0 2] op-mark]]

        [[[0 0] op-mark] [[0 1] mark] [[0 2] op-mark]]

        [[[0 0] op-mark] [[0 1] op-mark] [[0 2] mark]])))

  (testing "True positives"
    (doseq [mark ["x" "o"]]
      (are [result attack]
        (= (winning-attack? attack)
           result)

        [[0 0] mark] [[[0 0] " "] [[0 1] mark] [[0 2] mark]]

        [[0 1] mark] [[[0 0] mark] [[0 1] " "] [[0 2] mark]]

        [[0 2] mark] [[[0 0] mark] [[0 1] mark] [[0 2] " "]]))))

(deftest get-winning-move-test
  (testing "True negatives"
    (are [board]
        (= (get-winning-move board "x")
           nil)

        [" " " " " "
         " " " " " "
         " " " " " "]

        ["o" "x" "o"
         "o" "x" "o"
         "x" "o" "x"]

        ["o" "x" " "
         "x" " " "o"
         " " "o" "x"]))

  (testing "Simple positives"
    (doseq [mark ["x" "o"]
            op-mark ["o" "x"]]
      (are [result board]
          (= (get-winning-move board op-mark)
              result)

          [1 1] [" " mark mark
                 " " " " " "
                 " " " " " "]

          [3 1] [mark mark " "
                 " " " " " "
                 " " " " " "]

          [1 1] [" " " " " "
                 mark " " " "
                 mark " " " "]

          [1 3] [mark " " " "
                 mark " " " "
                 " " " " " "]

          [1 1] [" " " " " "
                 " " mark " "
                 " " " " mark]

          [3 3] [mark " " " "
                 " " mark " "
                 " " " " " "]

          [3 1] [" " " " " "
                 " " mark " "
                 mark " " " "]

          [1 3] [" " " " mark
                 " " mark " "
                 " " " " " "]))))

(def pos-inf Double/POSITIVE_INFINITY)
(def neg-inf Double/NEGATIVE_INFINITY)

(deftest minimax-test
  (testing "Completed boards"
    (are [result board]
      (= (minimax board :ai {:ai "o" :player "x"})
         result)
      pos-inf ["o" "x" "o"
               "o" "x" "x"
               "o" "o" "x"]

      neg-inf ["o" "x" "o"
               "o" "x" "x"
               "x" "x" "o"]

      0       ["x" "x" "o"
               "o" "o" "x"
               "x" "x" "o"]))

  (testing "One to complete boards"
    (doseq [board [["o" "x" "o"
                    "o" "x" "x"
                    "x" " " "o"]

                   [" " " " " "
                    "o" "x" " "
                    "x" " " " "]

                   [" " " " " "
                    "o" " " " "
                    "x" "x" " "]]]
      (are [result to-play marks]
        (= (minimax board to-play marks)
           result)
        neg-inf :player {:ai "o" :player "x"}
        pos-inf :ai     {:ai "x" :player "o"}))

    (doseq [board [["x" "o" "o"
                    "o" "x" "x"
                    " " "x" "o"]

                   ["x" "o" "o"
                    "o" "x" "x"
                    " " "x" "o"]

                   ["x" "o" "x"
                    " " "x" "o"
                    "o" "x" "o"]]]
      (are [result to-play marks]
        (= (minimax board to-play marks)
           result)
        0 :player {:ai "o" :player "x"}
        0 :ai     {:ai "x" :player "o"})))

  (testing "Fork handling"
    (doseq [board [["x" " " "x"
                    " " "o" " "
                    "o" " " "x"]

                   ["o" "o" "x"
                    " " "x" " "
                    " " " " "x"]

                   [" " "o" "x"
                    " " "x" "x"
                    "o" " " " "]

                   ["o" " " " "
                    " " "o" "x"
                    " " "x" "x"]]]
      (are [result to-play marks]
        (= (minimax board to-play marks)
           result)
        neg-inf :player {:ai "o" :player "x"}
        pos-inf :ai     {:ai "x" :player "o"})))

  (testing "Fork prediction"
    (doseq [board [["x" " " "o"
                    " " "o" " "
                    " " " " "x"]

                   [" " " " " "
                    " " "x" "o"
                    " " " " " "]

                   [" " "o" " "
                    " " "x" " "
                    "x" " " " "]

                   ["o" " " " "
                    " " " " " "
                    "x" "x" "o"]]]
      (are [result to-play marks]
        (= (minimax board to-play marks)
           result)
        neg-inf :player {:ai "o" :player "x"}
        pos-inf :ai     {:ai "x" :player "o"})))

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
