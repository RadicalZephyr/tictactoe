(ns tictactoe.cli
  (:require [tictactoe.ai :as ai]
            [tictactoe.board :as board]
            [clojure.pprint :as pp]
            [clojure.tools.reader.edn :as edn]))

(defn label-board [board]
  (->> (range 1 4)
       (map vector)
       (map concat
            (partition 3 board))))

(defn print-board [board]
  (pp/cl-format *out* "~:{+---+---+---+~%~
                          | ~C | ~C | ~C | ~D~%~}~
                          +---+---+---+~
                        ~%  1   2   3~%~%"
                ;; Print the board so that the lower-left corner is
                ;; 1,1 This conforms to the typical cartesian
                ;; coordinate system and so should be more intuitive
                ;; to understand.
                (-> board
                    label-board
                    reverse)))

(defn unknown-val [tag val]
  {:unknown-tag tag
   :value val})

(defn safe-read []
  (edn/read {:default unknown-val}
            *in*))

(defn num->xy [input]
  (->> input
       str
       seq  ;; This read string is safe because we KNOW it's a number!
       (map (comp edn/read-string
                  str))
       vec))

(defn validate-number-input [input]
  (case input
    (1 2 3) (let [y (safe-read)]
              (when (and (number? y)
                         (>= 3 y 1))
                [input y]))
    (11 12 13
        21 22 23
        31 32 33) (num->xy input)
        nil))

(defn is-move-collection? [input]
  (and (coll? input)
       (sequential? input)
       (= (count input)
          2)
       (every? (fn [x]
                 (and (number? x)
                      (> 4 x 0)))
               input)))

(defn valid-move-input? [input]
  (cond
    (number? input) (validate-number-input input)
    (is-move-collection? input) input
    :else nil))

(defn read-move []
  (print "Enter your move [x y]: ")
  (flush)
  (let [input (safe-read)]
    (if-let [move (valid-move-input? input)]
      move
      (do
        (println "I didn't understand that move.  Please try again.")
        (recur)))))

(defn make-ai-move [board to-play marks]
  (board/make-move board
                   (marks to-play)
                   (ai/best-ranked-move board marks)))

(defn make-player-move [board mark]
  (let [move (read-move)]
    (if (board/valid-move? board move)
      (board/make-move board mark move)
      (do
        (println "Sorry," move "is not a valid move.")
        (println "Please enter another move.")
        (recur board mark)))))

(defn next-move [board to-play marks]
  (print-board board)
  (case to-play
    :ai     (make-ai-move board to-play marks)
    :player (make-player-move board
                              (marks to-play))))

(defn game-loop [board to-play marks]
  (if-let [winner (or (board/which-winner? board)
                      (board/cats-game? board))]
    (do
      (print-board board)
      (cond
        (= (marks :player)
           winner)
        (println "How did this happen?!?!?!"
                 "The AI is suppposed to be UNBEATABLE!!!")

        (= (marks :ai)
           winner)
        (println "The AI wins again. As it should.")

        :else (println "It's a draw. This time...")))

    (recur (next-move board to-play marks)
           (board/next-player to-play)
           marks)))

(defn assign-marks [goes-first]
  {goes-first "X"
   (board/next-player goes-first) "O"})

(defn read-player []
  (print "Who should go first, player or ai? ")
  (flush)
  (loop []

    ;; Read something, keywordize it and then ensure that it's either
    ;; :player or :ai.  All other cases will end up nil. It's safe to call
    ;; keyword on pretty much any clojure data type.  Everything that
    ;; doesn't make sense just returns nil, and this allows you to accept
    ;; keywords, symbols or strings.
    (if-let [goes-first (-> (safe-read)
                            keyword
                            #{:player :ai})]
      goes-first
      (do
        (println "Sorry, I didn't understand that."
                 "Please enter either \"player\" or \"ai\".")
        (recur)))))

(defn play-again? []
  (print "Would you like to play again? ")
  (flush)
  (loop [again (safe-read)]
    (cond
      (#{'y 'yes} again) true
      (#{'n 'no}  again) false
      :else (do
              (println "Sorry, I didn't understand that."
                       "Please enter either \"yes\" or \"no\".")
              (recur (safe-read))))))

(defn start-game []
  (println "Let's play tictactoe!")
  (loop [goes-first (read-player)]
    (game-loop board/empty-board
               goes-first
               (assign-marks goes-first))

    (when (play-again?)
      (recur (read-player)))))
