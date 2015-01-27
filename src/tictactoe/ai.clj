(ns tictactoe.ai)

(defn- is-winning-move? [board [x y]])

(defn- has-winning-move? [board])

(defn- play-winning-move [board])

(defn- play-best-available-move [board])

(defn best-move [board]
  (cond
    (has-winning-move? board) (play-winning-move board)
    :else (play-best-available-move board)))
