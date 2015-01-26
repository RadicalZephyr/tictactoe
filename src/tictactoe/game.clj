(ns tictactoe.game
  (:gen-class)
  (:require [clojure.pprint :as pp]))

(defn print-board [board]
  (pp/cl-format *out* "~:{+---+---+---+~%~
                          | ~C | ~C | ~C |~%~}~
                          +---+---+---+~%"
                (partition 3 board)))


(defn -main [& args]
  (println "Hey it's tictactoe!"))
