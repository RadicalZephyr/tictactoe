(ns tictactoe.ai
  (:require [tictactoe.board :as board]))

(defn- is-winning-group? [indexed-row])

(defn- has-winning-move? [board]
  (->> board
       board/all-indexed-board-groups
       (map is-winning-group?)
       (filter identity)))

(defn- play-winning-move [board])

(defn- play-best-available-move [board])

(defn best-move [board]
  (cond
    (has-winning-move? board) (play-winning-move board)
    :else (play-best-available-move board)))
