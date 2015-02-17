(ns tictactoe.ai-test
  (:require [tictactoe.ai :refer :all]
            [clojure.test :refer :all]
            [tictactoe.board :as board]
            [tictactoe.cli :refer [game-loop]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.math.combinatorics :refer [permutations]]))

(def op-mark {"x" "o"
              "o" "x"})

;;; #################################################################
;;; Generators for non-indexed attacks
;;; #################################################################

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


;;; #################################################################
;;; Property tests for check-winning-move
;;; #################################################################

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


;;; #################################################################
;;; Unit tests for check-winning-move
;;; #################################################################

(deftest check-winning-move-test
  (testing "False positives"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [mark " " " "])]
        (is (= (check-winning-move attack)
               nil)))))

  (testing "True positives"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " mark mark])]
        (is (= (check-winning-move attack)
               mark)))))

  (testing "True negatives"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [mark mark (op-mark mark)])]
        (is (= (check-winning-move attack)
               nil))))))


;;; #################################################################
;;; Generator's for indexed attacks
;;; #################################################################

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


;;; #################################################################
;;; Property tests for winning-attack?
;;; #################################################################

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


;;; #################################################################
;;; Unit tests for winning-attack?
;;; #################################################################

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


;;; #################################################################
;;; Unit tests for get-winning-move
;;; #################################################################

(deftest get-winning-move-test
  (testing "True negatives"
    (doseq [mark ["x" "o"]]
      (are [board]
        (= (get-winning-move board mark)
           nil)

        [" " " " " "
         " " " " " "
         " " " " " "]

        ["o" "x" "o"
         "o" "x" "o"
         "x" "o" "x"]

        ["o" "x" " "
         "x" " " "o"
         " " "o" "x"])))

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


;;; #################################################################
;;; Unit tests for minimax
;;; #################################################################

(deftest minimax-test
  (testing "Completed boards"
    (are [result board]
      (= (minimax board :ai {:ai "o" :player "x"} 1)
         result)
      9 ["o" "x" "o"
         "o" "x" "x"
         "o" "o" "x"]

      -9 ["o" "x" "o"
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
        (= (minimax board to-play marks 1)
           result)
        -8 :player {:ai "o" :player "x"}
         8 :ai     {:ai "x" :player "o"}))

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
        (= (minimax board to-play marks 1)
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
        (= (minimax board to-play marks 1)
           result)
        -8 :player {:ai "o" :player "x"}
         8 :ai     {:ai "x" :player "o"})))

  (testing "Fork prediction"
    (doseq [board [["x" " " "o"
                    " " "o" " "
                    " " " " "x"]

                   ["o" " " " "
                    " " " " " "
                    "x" "x" "o"]]]
      (are [result to-play marks]
        (= (minimax board to-play marks 1)
           result)
        -6 :player {:ai "o" :player "x"}
         6 :ai     {:ai "x" :player "o"})))

  (testing "Immediate wins and losses are more important than forks"
    (doseq [board [[" " "o" " "
                    " " "x" " "
                    "x" " " " "]

                   [" " "o" " "
                    " " " " " "
                    "x" "x" " "]]]
      (are [result to-play marks]
        (= (minimax board to-play marks 1)
           result)
        -8 :player {:ai "o" :player "x"}
         8 :ai     {:ai "x" :player "o"})))

  (testing "Previous AI failure modes"
    (are [to-play board]
      (= (minimax board to-play {:ai "o" :player "x"} 1)
         0)

      :player
      ["x" "o" " "
       " " "o" " "
       " " " " "x"]

      :ai
      ["x" " " " "
       " " "o" " "
       " " " " "x"]))

  (testing "First moves"
    (are [board]
      (= (minimax board
                  :player {:ai "x" :player "o"} 1)
         0)

      [" " " " " "
       " " " " " "
       " " " " "x"]

      [" " " " " "
       " " " " " "
       " " "x" " "]

      [" " " " " "
       " " "x" " "
       " " " " " "]))

  (testing "Prefer immediate wins"))

;; For testing the game-driver
;; (defn swap-marks [{:keys [ai player]}]
;;   {:ai player
;;    :player ai})

(defn make-ai-move [board to-play marks]
  (board/make-move board
                   (marks to-play)
                   (best-minimax-move board marks)))

(def ^:dynamic move-list nil)

(defn make-test-move [board mark]
  (->> move-list
       (some (fn [i]
               (when (board/valid-move-i? board i)
                 i)))
       (board/make-move-i board mark)))

(defn next-move [board to-play marks]
  (case to-play
    ;; For testing the game-driver
    :ai     (make-test-move board
                            (marks to-play))
    ;; :ai     (make-ai-move board to-play marks)
    :player (make-test-move board
                            (marks to-play))))

(defn test-game-driver [plays-first move-list]
  (binding [move-list move-list]
    (game-loop (fn [board result] result) next-move
      [board/empty-board plays-first
       {plays-first "x"
        (board/next-player plays-first) "o"}])))

;; This test is for testing the above testing functions.

;; To make these tests pass, you must modify `next-move' to call
;; `make-test-move' for both players

(deftest test-game-driver-test
  (testing "The test fixtures"
    (are [result plays-first move-list]
      (= (test-game-driver plays-first move-list)
         result)

      :player :player [0 3 1 4 2 5]
      :ai     :player [0 3 1 4 6 5]

      :player :ai     [0 3 1 4 6 5]
      :ai     :ai     [0 3 1 4 2 5]

      :draw :player [0 1 2 4 3 6 5 8 7]
      :draw :ai     [0 1 2 4 3 6 5 8 7]

      :draw :player [0 1 2 3 5 4 6 8 7]
      :draw :ai     [0 1 2 3 5 4 6 8 7]

      :draw :player [0 1 2 3 4 6 5 8 7]
      :draw :ai     [0 1 2 3 4 6 5 8 7])))

(defn test-all-ai [plays-first]
  (->> (range 9)
       permutations
       (partition 75000)
       (pmap #(frequencies
               (map (fn [ml]
                      (test-game-driver plays-first ml)) %)))
       (apply merge-with +)))

(defn do-consistency-test []
  (for [i (range 4)]
    [(test-all-ai :player) (test-all-ai :ai)]))


;; This test is commented out because it takes a VERY long time to run.
;; Like, 10 minutes...

;; (deftest ai-consistency-test
;;   ((let [freq-seq (do-consistency-test)]
;;      (is (apply = freq-seq))

;;      (is (= (set (keys (first freq-seq)))
;;             #{:ai :draw})))))
