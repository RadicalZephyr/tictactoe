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

(defn- play-winning-move [board mark])

(defn- play-best-available-move [board mark])

(defn best-move [board mark]
  (or (play-winning-move board mark)
      (play-best-available-move board)))
