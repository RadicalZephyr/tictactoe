(ns tictactoe.ai
  (:require [tictactoe.board :as board]))

(defn- all-index-pairs []
  (board/all-board-groups (range 9)))

(defn- has-winning-move? [board])

(defn- play-winning-move [board])

(defn- play-best-available-move [board])

(defn best-move [board]
  (cond
    (has-winning-move? board) (play-winning-move board)
    :else (play-best-available-move board)))
