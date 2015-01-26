(ns tictactoe.cli
  (:require [clojure.pprint :as pp]
            [clojure.tools.reader.edn :as edn]))

(defn print-board [board]
  (pp/cl-format *out* "~:{+---+---+---+~%~
                          | ~C | ~C | ~C |~%~}~
                          +---+---+---+~%"
                (partition 3 board)))

(defn read-move []
  (print "Enter your move [x y]: ")
  (let [my-unknown (fn [tag val] {:unknown-tag tag
                                  :value val})
        input (edn/read {:default my-unknown
                         :eof nil} *in*)]
    (if (and (coll? input)
             (sequential? input)
             (= (count input)
                2)
             (every? number? input))
      (vec input)
      (do
        (println "I didn't understand that move.  Please try again.")
        (recur)))))

(defn winner? [board]
  (->>
   (concat
    ;; Horizontal groups
    (partition 3 board)

    ;; Vertical groups
    (for [x (range 3)]
      (take-nth 3 (drop x [1 2 3 4 5 6 7 8 9])))

    ;; top-right to bottom-left diagonal
    (->> board
         (drop 2)
         (take-nth 2)
         (take 3))

    ;; top-left to bottom-right diagonal
    (take-nth 4 board))
   (map (fn [section]
          (and
           (apply = section)
           (apply not= " " section))))
  (some identity)))

(defn next-move [board to-play]
  board)

(def next-player {:player :ai
                  :ai :player})

(defn game-loop [board to-play]
  (if-let [winner (winner? board)]
    (case winner
      :player (println "How did this happen?"
                       "The AI is suppposed to be UNBEATABLE?!?!?!")
      :ai (println "The AI wins again. As it should."))
    (recur (next-move board to-play)
           (next-player to-play))))
