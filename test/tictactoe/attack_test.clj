(ns tictactoe.attack-test
  (:require [tictactoe.attack :refer :all]
            [clojure.test :refer :all]
            [clojure.math.combinatorics :refer [permutations]]))

(def op-mark {"x" "o"
              "o" "x"})

(deftest classify-test
  (testing "Identify a potential"
    (is (= (classify [" " " " " "] "x")
           :potential)))
  (testing "Identify nulls"
    (doseq [attack (permutations [" " "o" "x"])]
      (is (= (classify attack  "x")
             :null))
      (is (= (classify attack  "o")
             :null))))
  (testing "Identify threats"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " " " mark])]
        (is (= (classify attack (op-mark mark))
               :threat)
            (str "Attack: " (vec attack) " and mark: " mark)))))
  (testing "Identify shots"
    (doseq [mark ["x" "o"]]
      (doseq [attack (permutations [" " " " mark])]
        (is (= (classify attack mark)
               :shot)
            (str "Attack: " (vec attack) " and mark: " mark))))))
