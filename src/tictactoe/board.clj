(ns tictactoe.board
  (:require [clojure.string :as s]))

(def blank " ")

(def empty-board
  (vec (repeat 9 blank)))

(defprotocol IndexedCell
  "A protocol for interacting with a single board cell."
  (index [cell])
  (mark  [cell]))

(defrecord ICell [_index _mark]
  java.lang.Comparable
  (compareTo [this o] (compare
                       (index this)
                       (index o)))

  IndexedCell
  (index [_] _index)
  (mark  [_] _mark))

(defn make-indexed-cell [index mark]
  (ICell. index mark))

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
  (map make-indexed-cell
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

(def index-equivalences
  {:lr    [#{0 2} #{3 5} #{6 8}]
   :tb    [#{0 6} #{1 7} #{2 8}]
   :tl-br [#{1 3} #{2 6} #{5 7}]
   :tr-bl [#{0 8} #{1 5} #{3 7}]})

(defn =at-index [board [a b]]
  (= (nth board a)
     (nth board b)))

(defn has-sym? [board sym-type]
  (when (every? (partial =at-index board)
                (map vec
                     (index-equivalences sym-type)))
    sym-type))

(defn has-lr-sym? [board]
  (has-sym? board :lr))

(defn has-tb-sym? [board]
  (has-sym? board :tb))

(defn has-tl-br-sym? [board]
  (has-sym? board :tl-br))

(defn has-tr-bl-sym? [board]
  (has-sym? board :tr-bl))

(def all-syms
  (juxt has-lr-sym?
        has-tb-sym?
        has-tl-br-sym?
        has-tr-bl-sym?))

(defn get-symmetries [board]
  (set
   (keep identity
         (all-syms board))))

(defn classify-index [index]
  (case index
    (0 2 6 8) :corner
    (1 3 5 7) :side
    4         :center
    nil))

(defn get-unique-moves [board]
  #{0 1 4})
