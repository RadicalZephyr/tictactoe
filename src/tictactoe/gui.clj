(ns tictactoe.gui
  (:require [seesaw.core :as s]))

(def root (atom nil))

(defn get-canvas []
  (s/select @root [:#canvas]))

(defn show-frame [frame]
  (s/invoke-later
   (-> frame
       s/show!)))

(defn -main [& args]
  (compare-and-set!
   root nil (s/frame :title "Tic-Tac-Toe"
                     :size [640 :by 480]
                     :content (s/canvas :id :canvas)))
  (show-frame @root))
