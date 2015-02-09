(ns tictactoe.board
  (:require [clojure.string :as s]))

(def blank " ")

(def empty-board
  (vec (repeat 9 blank)))

(defprotocol IndexedCell
  "A protocol for interacting with a single board cell."
  (index [cell])
  (mark  [cell]))

(defn make-indexed-cell [index mark]
  [index mark])

(extend-type clojure.lang.IPersistentVector
  IndexedCell
  (index [cell] (first cell))
  (mark  [cell] (second cell)))

(defn horizontal-attacks [board]
  (partition 3 board))

(defn vertical-attacks [board]
  (for [x (range 3)]
    (take-nth 3 (drop x board))))

(defn top-left->bottom-right-attack [board]
  (take-nth 4 board))

(defn top-right->bottom-left-attack [board]
  (->> board
       (drop 2)
       (take-nth 2)
       (take 3)))

(defn diagonal-attacks [board]
  [(top-left->bottom-right-attack board)
   (top-right->bottom-left-attack board)])

(defn all-attacks [board]
  (concat
   (horizontal-attacks board)
   (vertical-attacks board)
   (diagonal-attacks board)))

(defn indexed-board [board]
  (map vector
       (for [y (range 1 4)
             x (range 1 4)]
         [x y])
       board))

(defn all-indexed-attacks [board]
  (->> board
       indexed-board
       all-attacks))

(defn winning-attack? [attack]
  (and
   (apply = attack)
   (apply not= blank attack)))

(defn which-winner? [board]
  (->> board
       all-attacks

       ;; Check all different combinations for a winning combination
       (some (fn [attack]
               (if (winning-attack? attack)
                 (first attack)
                 nil)))))

(defn cats-game? [board]
  (not-any? s/blank? board))

(defn xy->index [[x y]]
  (+ (dec x)
     (* 3 (dec y))))

(defn index->xy [index]
  [(inc (rem index 3))
   (inc (quot index 3))])

(defn make-move-i [board mark index]
  (if (= (nth board index)
         blank)
    (assoc board index mark)
    (throw (ex-info "Illegal move." {:position (index->xy index)
                                     :board board}))))

(defn make-move [board mark pos]
  (make-move-i board mark (xy->index pos)))

(defn get-pos [board pos]
  (get board (xy->index pos)))

(defn valid-move-i? [board index]
  (= (get board index)
     blank))

(defn valid-move? [board pos]
  (valid-move-i? board (xy->index pos)))


(def next-player {:player :ai
                  :ai :player})
