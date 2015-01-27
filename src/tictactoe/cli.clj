(ns tictactoe.cli
  (:require [tictactoe.ai :as ai]
            [clojure.pprint :as pp]
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
      (take-nth 3 (drop x board)))

    ;; top-left to bottom-right diagonal
    [(take-nth 4 board)
     ;; top-right to bottom-left diagonal
     (->> board
          (drop 2)
          (take-nth 2)
          (take 3))])

   ;; Check all different combinations for a winning combination
   (map (fn [section]
          (if (and
               (apply = section)
               (apply not= " " section))
            (first section)
            nil)))

  (some identity)))

(defn make-move [board mark [x y]]
  (let [pos (+ (dec x)
               (* 3 (dec y)))]
    (if (= (nth board pos)
           " ")
      (assoc board pos mark)
      (throw (ex-info "Illegal move." {:position pos
                                       :board board})))))

(defn make-ai-move [board mark]
  (make-move board mark (ai/best-move board)))

(defn next-move [board to-play mark]
  (case to-play
    :ai (make-ai-move board mark)
    :player (make-move board (mark) (read-move))))

(def next-player {:player :ai
                  :ai :player})

(defn game-loop [board to-play marks]
  (if-let [winner (winner? board)]
    (case winner
      :player (println "How did this happen?!?!?!"
                       "The AI is suppposed to be UNBEATABLE!!!")
      :ai (println "The AI wins again. As it should."))
    (recur (next-move board to-play (marks to-play))
           (next-player to-play)
           marks)))
