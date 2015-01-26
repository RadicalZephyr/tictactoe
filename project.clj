(defproject tictactoe "0.1.0-SNAPSHOT"
  :description "A tictactoe game with an UNBEATABLE AI."
  :url "https://www.github.com/radicalzephyr/tictactoe"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.reader "0.8.13"]]
  :main ^:skip-aot tictactoe.game
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
