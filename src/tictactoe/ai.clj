(ns tictactoe.ai
  (:require [tictactoe.board :as board
                             :refer [blank]]
            [tictactoe.attack :refer [rank-moves]]))

(defn check-winning-move [attack]
  (let [freqs (frequencies attack)]
    (when (and (= (count freqs)
                  2)
               (= (freqs blank)
                  1))
      (->> freqs
           keys
           (some (fn [el]
                   (when (not (#{" "} el))
                     el)))))))

(defn winning-attack? [indexed-attack]
  (let [raw-attack (map board/mark indexed-attack)]
    (when-let [move (check-winning-move raw-attack)]
      (some (fn [cell]
              (when (= blank (board/mark cell))
                [(board/index cell) move]))
            indexed-attack))))

(defn get-winning-move [board mark]
  (let [winning-moves
        (->> board
             board/all-indexed-attacks
             (map winning-attack?)
             (filter identity)
             (group-by second))]
    (when (not (empty? winning-moves))
      (cond (contains? winning-moves mark)
            (board/index (get-in winning-moves [mark 0]))

            :else (->> winning-moves
                       vals
                       first ; Get the first winning move list
                       first ; Get the first move off that list
                       board/index)))))


;;; #################################################################
;;; The most basic naive heuristic for playing tic-tac-toe
;;; #################################################################

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


;;; #################################################################
;;; Better performance than the naive heuristic, but still makes bad
;;; moves.  Generally does fairly well except in predicting the
;;; opponent creating a fork.
;;; #################################################################

(defn best-ranked-move [board marks]
  (let [{my-mark :ai
         other-mark :player} marks]
    (->>
     (rank-moves board my-mark other-mark)
     (sort (fn [[p1 v1] [p2 v2]]
             (compare v2 v1)))
     first
     ((fn [[pos ranking]]
        pos)))))


;;; #################################################################
;;; AI that uses the minimax decision algorithm.  Should be impossible
;;; to beat.
;;; #################################################################

;; Pseudocode:

;; function minimax(node, depth, maximizingPlayer)
;;   if depth = 0 or node is a terminal node
;;     return the heuristic value of node
;;   if maximizingPlayer
;;     bestValue := -∞
;;     for each child of node
;;       val := minimax(child, depth - 1, FALSE)
;;       bestValue := max(bestValue, val)
;;     return bestValue
;;   else
;;     bestValue := +∞
;;     for each child of node
;;       val := minimax(child, depth - 1, TRUE)
;;       bestValue := min(bestValue, val)
;;     return bestValue

(defn all-valid-moves [board]
  (->> (range 9)
       (filter (partial board/valid-move-i? board))))

(defn all-sequential-moves [mark board]
  (->> board
       all-valid-moves
       (map (partial board/make-move-i board mark))))

(defn minimax [board player marks]
  (if-let [winner (or
                   (board/which-winner? board)
                   (board/cats-game? board))]
    (cond
      (= (marks :ai) winner)     Double/POSITIVE_INFINITY
      (= (marks :player) winner) Double/NEGATIVE_INFINITY
      :else 0)

    (if-let [subtree-values (->> board
                                 (all-sequential-moves (marks player))
                                 (map (fn [b]
                                        (minimax b
                                                 (board/next-player player)
                                                 marks))))]

      (case player
        :ai     (apply max subtree-values)
        :player (apply min subtree-values)))))

(defn minimax-rank-move [board marks index]
  [index (minimax (board/make-move-i board (marks :ai) index) :player marks)])

(defn best-minimax-move [board marks]
  (->> board
       all-valid-moves
       (map (partial minimax-rank-move board marks))
       (sort (fn [[_ r1] [_ r2]]
               (compare r2 r1)))
       first
       ((fn [[index _]]
          (board/index->xy index)))))
