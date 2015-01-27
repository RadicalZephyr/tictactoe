(ns tictactoe.ai
  (:require [tictactoe.board :as board]))

(defn check-winning-move [row]
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
    (when-let [move (check-winning-move raw-row)]
      (some (fn [[pos val]]
              (when (= " " val)
                [pos move]))
            indexed-row))))

(defn get-winning-move [board mark]
  (let [winning-moves
        (->> board
              board/all-indexed-board-groups
              (map winning-group)
              (filter identity)
              (group-by second))]
    (when (not (empty? winning-moves))
      (cond (contains? winning-moves mark)
            (get-in winning-moves [mark 0 0])

            :else (->> winning-moves ;; This is ugly! What to do about
                       vals          ;; it though?
                       first
                       first
                       first)))))

(defn- play-winning-move [board mark]
  (when-let [move (get-winning-move board mark)]
    (board/make-move board mark move)))

(def move-ranking
  "The priority ordering of all the different moves in the
  game. Basically, the corners are superior to the middle which is
  superior to the side moves.  This has to do with the number of
  potential paths that each space contributes to.  Corners contribute
  three potential paths, the middle has four, and the sides have two
  each."
  [[1 1] [1 3] [3 1] [3 3] [2 2] [1 2] [2 1] [3 2] [2 3]])

(defn not-taken [board pos]
  (when (= (board/get-pos board pos)
           " ")
    pos))

(defn- play-best-available-move [board mark]
  (->>
   move-ranking
   (some (partial not-taken board))
   (board/make-move board mark)))

(defn best-move [board marks]
  (let [my-mark (marks :ai)]
      (or (play-winning-move board my-mark)
          (play-best-available-move board my-mark))))
