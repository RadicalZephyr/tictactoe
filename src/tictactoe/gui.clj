(ns tictactoe.gui
  (:require  [tictactoe.ai :as ai]
             [tictactoe.board :as board]
             [seesaw.core :as s]
             [seesaw.bind :as b]
             [seesaw.graphics :as g]))

(def root (atom nil))

(def board (atom board/empty-board))

(defn get-canvas []
  (s/select @root [:#canvas]))

(defn show-frame [frame]
  (s/invoke-later
   (-> frame
       s/show!)))

(defn fit-grid-to-screen [padding [screen-w screen-h]]
  (let [grid-w 3
        grid-h 3
        padcount-w (inc grid-w)
        padcount-h (inc grid-h)
        rect-width (- screen-w
                      (* padding
                         padcount-w))
        rect-height (- screen-h
                       (* padding
                          padcount-h))]
    [(quot rect-width  grid-w)
     (quot rect-height grid-h)
     (rem rect-width  grid-w)
     (rem rect-height grid-h)]))

(defn draw-board [c g]
  (let [[w h]]
   (g/draw g
           (g/rect 10 10 10 10)
           (g/style :foreground :black
                    :background nil))))

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
