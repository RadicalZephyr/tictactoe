(defproject tictactoe "0.2.1"
  :description "A tictactoe game with an UNBEATABLE AI."
  :url "http://www.zephyrizing.net/tictactoe/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.reader "1.2.2"]
                 [seesaw "1.4.5"]]
  :main ^:skip-aot tictactoe.game
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/math.combinatorics "0.1.4"]
                                  [org.clojure/test.check "0.9.0"]]}})
