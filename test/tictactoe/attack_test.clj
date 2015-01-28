(ns tictactoe.attack-test
  (:require [tictactoe.attack :refer :all]
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
  (testing "Identify shots"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " " " mark])]
        (is (= (classify attack mark (op-mark mark))
               :shot)
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
