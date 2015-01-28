(ns tictactoe.board
  (:require [clojure.string :as s]))

(def blank " ")

(def empty-board (vec (repeat 9 blank)))

(defn all-attacks [board]
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
       (for [x (range 1 4)
             y (range 1 4)]
         [y x])
       board))

(defn all-indexed-attacks [board]
  (->> board
       indexed-board
       all-attacks))

(defn winner? [board]
  (->>
   (all-attacks board)

   ;; Check all different combinations for a winning combination
   (some (fn [section]
           (if (and
                (apply = section)
                (apply not= blank section))
             (first section)
             nil)))))

(defn cats-game? [board]
  (not-any? s/blank? board))

(defn xy->index [[x y]]
  (+ (dec x)
     (* 3 (dec y))))

(defn index->xy [index]
  [(inc (rem index 3))
   (inc (quot index 3))])

(defn make-move [board mark pos]
  (let [index (xy->index pos)]
    (if (= (nth board index)
           blank)
      (assoc board index mark)
      (throw (ex-info "Illegal move." {:position pos
                                       :board board})))))
(defn get-pos [board pos]
  (get board (xy->index pos)))

(defn valid-move? [board pos]
  (= (get-pos board pos)
     blank))

(def next-player {:player :ai
                  :ai :player})
