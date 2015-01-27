(ns tictactoe.util)

(defn all-board-groups [board]
  (concat
   ;; Horizontal groups
   (partition 3 board)

   ;; Vertical groups
   (for [x (range 3)]
     (take-nth 3 (drop x board)))

   ;; top-left to bottom-right diagonal
   [(take-nth 4 board)

    ;; top-right to bottom-left diagonal
    (->> board
         (drop 2)
         (take-nth 2)
         (take 3))]))
