(ns tictactoe.ai
  (:require [tictactoe.board :as board
                             :refer [blank]]
            [tictactoe.attack :refer [classify]]))

;; Tic-tac-toe game analysis: There is only one fundamental thing in
;; tic-tac-toe and that is a three-in-a-row.  We will call this an
;; "attack".

;; Now, there are four different types of attacks.  There are
;; potential attacks, threats, shots, and void's. These
;; distinctions are simply descriptive of which player has claimed
;; that attack.

;; A potential attack is one where no player has claimed any of the
;; squares in it.

;; A null attack is one where no player can claim because both players
;; have played in the attack at least once.

;; A threat is one in which the opposing player has claimed that
;; attack by placing at least one piece in it, and the other's are
;; unclaimed. A shot is the opposite, where the ai has claimed that
;; attack. Both threats and shots can have a priority, which is
;; whether we have claimed two of the slots in the attack or only one.


(defn check-winning-move [row]
  (let [freqs (frequencies row)]
    (when (and (= (count freqs)
                2)
               (= (freqs blank)
                  1))
      (->> freqs
           keys
           (remove #{blank})
           first))))

(defn winning-group [indexed-row]
  (let [raw-row (map second indexed-row)]
    (when-let [move (check-winning-move raw-row)]
      (some (fn [[pos val]]
              (when (= blank val)
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
           blank)
    pos))

(defn- get-best-available-move [board mark]
  (some (partial not-taken board)
        move-ranking))

(defn best-move [board marks]
  (let [my-mark (marks :ai)]
      (or (get-winning-move board my-mark)
          (get-best-available-move board my-mark))))
