(ns tictactoe.board)

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

(defn indexed-board [board]
  (map vector
       (for [x (range 3)
             y (range 3)]
         [x y])
       board))

(defn all-indexed-board-groups [board]
  (->> board
       indexed-board
       all-board-groups))

(defn winner? [board]
  (->>
   (all-board-groups board)

   ;; Check all different combinations for a winning combination
   (some (fn [section]
           (if (and
                (apply = section)
                (apply not= " " section))
             (first section)
             nil)))))

(defn make-move [board mark [x y]]
  (let [pos (+ (dec x)
               (* 3 (dec y)))]
    (if (= (nth board pos)
           " ")
      (assoc board pos mark)
      (throw (ex-info "Illegal move." {:position pos
                                       :board board})))))
