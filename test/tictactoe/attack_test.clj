(ns tictactoe.attack-test
  (:require [tictactoe.attack :refer :all]
            [tictactoe.board :as board]
            [clojure.test :refer :all]
            [clojure.math.combinatorics :refer [permutations]]))

(def op-mark {"x" "o"
              "o" "x"})

(deftest classify-test
  (testing "Identify a potential"
    (is (= (classify [" " " " " "] "x" "o")
           :potential)))
  (testing "Identify nulls"
    (doseq [attack (permutations [" " "o" "x"])]
      (is (= (classify attack  "x" "o")
             :null))
      (is (= (classify attack  "o" "x")
             :null)))
    (doseq [attack (permutations ["x" "o" "x"])]
      (is (= (classify attack  "x" "o")
             :null))
      (is (= (classify attack  "o" "x")
             :null)))
    (doseq [attack (permutations ["o" "o" "x"])]
      (is (= (classify attack  "x" "o")
             :null))
      (is (= (classify attack  "o" "x")
             :null))))
  (testing "Identify threats"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " " " mark])]
        (is (= (classify attack (op-mark mark) mark)
               :threat)
            (str "Attack: " (vec attack) " and mark: " mark)))))
  (testing "Identify losses"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " mark mark])]
        (is (= (classify attack (op-mark mark) mark)
               :loss)
            (str "Attack: " (vec attack) " and mark: " mark)))))
  (testing "Identify shots"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " " " mark])]
        (is (= (classify attack mark (op-mark mark))
               :shot)
            (str "Attack: " (vec attack) " and mark: " mark)))))
  (testing "Identify wins"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " mark mark])]
        (is (= (classify attack mark (op-mark mark))
               :win)
            (str "Attack: " (vec attack) " and mark: " mark))))))

(deftest get-space-frequencies-test
  (testing "Output"
    (is (= (get-space-frequencies
            '[([[1 1] " "] [[2 1] " "] [[3 1] " "])
              ([[1 2] " "] [[2 2] " "] [[3 2] " "])
              ([[1 3] " "] [[2 3] " "] [[3 3] " "])
              ([[1 1] " "] [[1 2] " "] [[1 3] " "])
              ([[2 1] " "] [[2 2] " "] [[2 3] " "])
              ([[3 1] " "] [[3 2] " "] [[3 3] " "])
              ([[1 1] " "] [[2 2] " "] [[3 3] " "])
              ([[3 1] " "] [[2 2] " "] [[1 3] " "])])
           {[1 1] 3 [1 2] 2 [1 3] 3
            [2 1] 2 [2 2] 4 [2 3] 2
            [3 1] 3 [3 2] 2 [3 3] 3}))))

(deftest invert-stat-position-test
  (testing "Output"
    (doseq [key [:potential :shot :threat]]
     (is (= (sort
             (invert-stat-position
              [key
               {[1 1] 3 [1 2] 2 [1 3] 3
                [2 1] 2 [2 2] 4 [2 3] 2
                [3 1] 3 [3 2] 2 [3 3] 3}]))
            (sort
             `([[1 1] {~key 3}] [[1 2] {~key 2}] [[1 3] {~key 3}]
               [[2 1] {~key 2}] [[2 2] {~key 4}] [[2 3] {~key 2}]
               [[3 1] {~key 3}] [[3 2] {~key 2}] [[3 3] {~key 3}])))))))

(deftest rank-spaces-test
  (testing "Output"
    (is (= (rank-spaces board/empty-board "x" "o")
           '([[2 2] {:potential 4}] [[2 3] {:potential 2}] [[3 3] {:potential 3}]
             [[1 1] {:potential 3}] [[1 3] {:potential 3}] [[3 1] {:potential 3}]
             [[2 1] {:potential 2}] [[1 2] {:potential 2}] [[3 2] {:potential 2}])))
    (is (= (sort
            (rank-spaces [" " " " "x"
                          " " "o" "x"
                          " " "o" " "]
                         "x" "o"))
           (sort
            '([[3 3] {:win 1, :threat 2}]
              [[1 1] {:shot 1, :potential 1, :threat 1}]
              [[1 3] {:potential 1, :null 1, :threat 1}]
              [[2 1] {:shot 1, :loss 1}]
              [[1 2] {:potential 1, :null 1}]))))))
