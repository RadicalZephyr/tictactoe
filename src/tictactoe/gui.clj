(ns tictactoe.gui
  (:require [tictactoe.ai :as ai]
            [tictactoe.board :as board]
            [seesaw.core :as s]
            [seesaw.bind :as b]
            [seesaw.graphics :as g]))

(def root (atom nil))

(def board (atom board/empty-board))

(def grid-rects (atom nil))

(def playing (atom true))

(def marks {:ai "x"
            :player "o"})

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
  (let [min-x (.getMinX rect)
        min-y (.getMinY rect)
        max-x (.getMaxX rect)
        max-y (.getMaxY rect)
        width (.getWidth rect)
        height (.getHeight rect)]
    (case letter
      "x" (do
            (.draw g2d (java.awt.geom.Line2D$Float.
                        (+ (* 0.1 width) min-x)
                        (- max-y (* 0.1 height))
                        (- max-x (* 0.1 height))
                        (+ (* 0.1 width) min-y)))
            (.draw g2d (java.awt.geom.Line2D$Float.
                        (+ (* 0.1 width) min-x)
                        (+ (* 0.1 height) min-y)
                        (- max-x (* 0.1 width))
                        (- max-y (* 0.1 height)))))
      "o" (g/draw g2d (g/ellipse (+ (* 0.1 width) min-x)
                                 (+ (* 0.1 height) min-y)
                                 (* 0.8 width)
                                 (* 0.8 height))
                  style))))

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

(defn keep-playing? []
  (when (and @playing
             (or
              (board/winner? @board)
              (board/cats-game? @board)))
    (swap! playing not))

  ;; This is a bit janky. End game is sort of overloaded with
  ;; functionality, because it has this side-effect of changing the
  ;; game state.  But in a certain way it's return value should ALWAYS
  ;; be whether or not we are continuing to play the game.
  @playing)

(defn reset-board! []
  (swap! playing not)
  (reset! board board/empty-board))

(defn mouse-click [e]
  (if (keep-playing?)
   (let [pt (.getPoint e)
         rects (map (fn [r]
                      (.contains r pt))
                    @grid-rects)
         click-index (.indexOf rects true)]
     (when (board/valid-move-i? @board click-index)
       (swap! board board/make-move-i (marks :player) click-index)
       (when (keep-playing?)
         (swap! board (fn [board]
                        (board/make-move board (marks :ai)
                                         (ai/best-ranked-move board marks)))))))
   (reset-board!)))

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
                     :on-close :exit
                     :content
                     (s/border-panel
                      :center
                      (s/canvas :id :canvas
                                :paint draw-board))))
  (show-frame @root))
