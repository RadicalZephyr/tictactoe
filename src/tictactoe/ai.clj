(ns tictactoe.ai
  (:require [tictactoe.board :as board]))

(defn winning-move [row]
  (let [all-but-one (dec (count row))
        freqs (frequencies row)]
    (when (and (= (count freqs)
                2)
             (contains? freqs " "))
      (->> freqs
           keys
           (remove #{" "})
           first))))

(defn- is-winning-group? [indexed-row]
  (let [raw-row (map second indexed-row)]
    (if-let [move (winning-move raw-row)]
      nil)))

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
