(ns tictactoe.gui
  (:require [tictactoe.ai :as ai]
            [tictactoe.board :as board]
            [seesaw.core :as s]
            [seesaw.bind :as b]
            [seesaw.graphics :as g]))

(def root (atom nil))

(def board (atom board/empty-board))

(def grid-rects (atom nil))

(defn get-canvas []
  (s/select @root [:#canvas]))

(defn fit-grid-to-screen [padding [grid-w grid-h] [screen-w screen-h]]
  (let [padcount-w (inc grid-w)
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

(defn get-coord [padding size x]
  (+ padding (* size x)))

(defn grid->rect [[pw ph] [w h] [x-len y-len]]
  (let [coord-x (partial get-coord pw w)
        coord-y (partial get-coord ph h)]
    (for [y (range y-len)
          x (range x-len)]
      (g/rect (coord-x x) (coord-y y)
              w           h))))

(defn get-size [frame]
  (let [dim (s/config frame :size)
        w (.width dim)
        h (.height dim)]
    [w h]))

(defn get-grid-rects [root]
  (let [dim [3 3]
        [w h slop-w slop-h] (fit-grid-to-screen
                             10 dim
                             (get-size root))]
    (grid->rect [20 10] [w h] dim)))


(defn draw-letter [g2d rect letter style]
  (case letter
    "x" (do
          (.draw g2d (java.awt.geom.Line2D$Float.
                      (+ (* 0.1 (.getWidth rect))
                         (.getMinX rect))
                      (- (.getMaxY rect)
                         (* 0.1 (.getHeight rect)))
                      (- (.getMaxX rect)
                         (* 0.1 (.getHeight rect)))
                      (+ (* 0.1 (.getWidth rect))
                         (.getMinY rect))))
          (.draw g2d (java.awt.geom.Line2D$Float.
                      (+ (* 0.1 (.getWidth rect))
                       (.getMinX rect))
                      (+ (* 0.1 (.getHeight rect))
                       (.getMinY rect))
                      (- (.getMaxX rect)
                       (* 0.1 (.getWidth rect)))
                      (- (.getMaxY rect)
                       (* 0.1 (.getHeight rect))))))
    "o" (g/draw g2d (g/ellipse (+ (* 0.1 (.getWidth  rect))
                                  (.getMinX   rect))
                               (+ (* 0.1 (.getHeight rect))
                                  (.getMinY   rect))
                               (* 0.8 (.getWidth  rect))
                               (* 0.8 (.getHeight rect)))
                style)))

(defn draw-board [canvas g2d]
  (let [root (s/to-root canvas)
        rect-style (g/style :foreground "black"
                            :stroke (g/stroke
                                     :width 5))
        rects (get-grid-rects root)]
    (reset! grid-rects rects)
    (dorun
     (map (fn [r mark]
            (g/draw g2d r rect-style)
            (when (not= mark " ")
              (draw-letter
               g2d r mark rect-style)))
          rects
          @board))))

(defn mouse-click [e]
  (let [pt (.getPoint e)
        rects (map (fn [r]
                     (.contains r pt))
                   @grid-rects)
        click-index (.indexOf rects true)]
    (when (board/valid-move-i? @board click-index)
      (swap! board board/make-move-i "o" click-index)
      (swap! board (fn [board]
                     (board/make-move board "x"
                      (ai/best-ranked-move board {:ai "x"
                                                  :player "o"})))))))

(defn show-frame [frame]
  (s/invoke-later
   (-> frame
       s/show!)
   (b/bind board (b/b-do [_]
                     (s/repaint! (get-canvas))))
   (s/listen (get-canvas)
       :mouse-released mouse-click)))

(defn -main [& args]
  (compare-and-set!
   root nil (s/frame :title "Tic-Tac-Toe"
                     :size [600 :by 480]
                     :content
                     (s/border-panel
                      :center
                      (s/canvas :id :canvas
                                :paint draw-board))))
  (show-frame @root))
