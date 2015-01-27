(ns tictactoe.ai
  (:require [tictactoe.board :as board]))

(defn winning-move [row]
  (let [freqs (frequencies row)]
    (when (and (= (count freqs)
                2)
               (= (freqs " ")
                  1))
      (->> freqs
           keys
           (remove #{" "})
           first))))

(defn winning-group [indexed-row]
  (let [raw-row (map second indexed-row)]
    (when-let [move (winning-move raw-row)]
      (some (fn [[pos val]]
              (when (= " " val)
                [pos move])) indexed-row))))

(defn- has-winning-move? [board]
  (->> board
       board/all-indexed-board-groups
       (map winning-group)
       (filter identity)))

(defn- play-winning-move [board])

(defn- play-best-available-move [board])

(defn best-move [board]
  (cond
    (has-winning-move? board) (play-winning-move board)
    :else (play-best-available-move board)))
