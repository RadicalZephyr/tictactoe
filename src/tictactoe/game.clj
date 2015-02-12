(ns tictactoe.game
  (:gen-class)
  (:require [tictactoe.cli :as cli]
            [tictactoe.gui :as gui]))

(defn print-usage []
  (println "tictactoe: [interface]\n")
  (println "Play a friendly game of tictactoe against a completely")
  (println "unbeatable AI. Don't feel bad though. No one can beat it.")
  (println "  -h --help\tPrint this message")
  (println "  -g --gui\tGraphical interface. Complete with button(s)!")
  (println "  -c --cli\tCommand-line/text interface.  No buttons though."))

(defn -main [& args]
  (if (seq args)
    (case (first args)
      ("-h" "--help") (print-usage)
      ("-g" "--gui")  (gui/-main)
      ("-c" "--cli")  (cli/-main)
      (print-usage))
    (cli/-main)))
