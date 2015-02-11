(defproject tictactoe "0.2.0-SNAPSHOT"
  :description "A tictactoe game with an UNBEATABLE AI."
  :url "http://www.zephyrizing.net/tictactoe/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.reader "0.8.13"]
                 [seesaw "1.4.5"]]
  :main ^:skip-aot tictactoe.game
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/math.combinatorics "0.0.8"]
                                  [org.clojure/test.check "0.7.0"]]}})
