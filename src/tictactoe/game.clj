(ns tictactoe.game
  (:gen-class)
  (:require [tictactoe.cli :as cli]))

(defn -main [& args]
  (cli/start-game))
