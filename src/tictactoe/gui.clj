(ns tictactoe.gui
  (:require  [tictactoe.ai :as ai]
             [tictactoe.board :as board]
             [seesaw.core :as s]
             [seesaw.bind :as b]))

(def root (atom nil))

(def board (atom board/empty-board))

(defn get-canvas []
  (s/select @root [:#canvas]))

(defn show-frame [frame]
  (s/invoke-later
   (-> frame
       s/show!)))

(defn draw-board [c g])


(defn mouse-click [e])

(defn -main [& args]
  (compare-and-set!
   root nil (s/frame :title "Tic-Tac-Toe"
                     :size [640 :by 480]
                     :content (s/canvas :id :canvas
                                        :paint draw-board)))
  (show-frame @root)
  (b/bind board (b/b-do [_]
                    (s/repaint! (get-canvas))))
  (s/listen (get-canvas)
      :mouse-clicked mouse-click))
