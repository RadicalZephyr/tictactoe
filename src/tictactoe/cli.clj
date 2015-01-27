(ns tictactoe.cli
  (:require [tictactoe.ai :as ai]
            [tictactoe.board :as board]
            [clojure.pprint :as pp]
            [clojure.tools.reader.edn :as edn]))

(defn print-board [board]
  (pp/cl-format *out* "~:{+---+---+---+~%~
                          | ~C | ~C | ~C |~%~}~
                          +---+---+---+~%"
                (partition 3 board)))

(defn unknown-val [tag val]
  {:unknown-tag tag
   :value val})

(defn read-move []
  (print "Enter your move [x y]: ")
  (let [input (edn/read {:default unknown-val}
                        *in*)]
    (if (and (coll? input)
             (sequential? input)
             (= (count input)
                2)
             (every? number? input))
      (vec input)
      (do
        (println "I didn't understand that move.  Please try again.")
        (recur)))))

(defn make-ai-move [board mark]
  (board/make-move board mark (ai/best-move board)))

(defn next-move [board to-play mark]
  (case to-play
    :ai (make-ai-move board mark)
    :player (board/make-move board mark (read-move))))

(def next-player {:player :ai
                  :ai :player})

(defn game-loop [board to-play marks]
  (if-let [winner (board/winner? board)]
    (case winner
      :player (println "How did this happen?!?!?!"
                       "The AI is suppposed to be UNBEATABLE!!!")
      :ai (println "The AI wins again. As it should."))
    (recur (next-move board to-play (marks to-play))
           (next-player to-play)
           marks)))

(defn assign-marks [goes-first]
  {goes-first "x"
   (next-player goes-first) "o"})

(defn start-game []
  (println "Let's play tictactoe!")
  (println "Who should go first, player or ai?")

;; Read something, keywordize it and then ensure that it's either
;; :player or :ai.  All other cases will end up nil. It's safe to call
;; keyword on pretty much any clojure data type.  Everything that
;; doesn't make sense just returns nil, and this allows you to accept
;; keywords, symbols or strings.
  (if-let [goes-first (-> (edn/read {:default unknown-val}
                                    *in*)
                          keyword
                          #{:player :ai})]
    (game-loop board/empty-board
               goes-first
               (assign-marks goes-first))
    (do
      (println "Sorry, I didn't understand that."
               "Please only enter \"player\" or \"ai\".")
      (recur))))
