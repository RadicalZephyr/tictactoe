(ns tictactoe.ai
  (:require [tictactoe.board :as board]))

(defn- has-winning-move? [row]
  (let [all-but-one (dec (count row))]
    (->> (frequencies row)
         (some (fn [[k v]]
                (if (= v all-but-one)
                  k nil))))))

(defn- play-winning-move [board])

(defn- play-best-available-move [board])

(defn best-move [board]
  (cond
    (has-winning-move? board) (play-winning-move board)
    :else (play-best-available-move board)))
