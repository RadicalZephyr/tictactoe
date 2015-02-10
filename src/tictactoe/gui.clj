(ns tictactoe.gui
  (:require [tictactoe.ai :as ai]
            [tictactoe.board :as board]
            [seesaw.core :as s]
            [seesaw.bind :as b]
            [seesaw.graphics :as g]))

(def root (atom nil))

(def starting-board-state {:board board/empty-board
                           :playing? true
                           :to-play :player})

(def game-state (atom starting-board-state))

(def marks {:ai "o"
            :player "x"})


;;; #################################################################
;;; Utility Functions
;;; #################################################################

(defn reset-board! []
  (reset! game-state starting-board-state))

(defn get-canvas [frame]
  (s/select frame [:#canvas]))

(defn toggle-playing [state]
  (update-in state [:playing?] not))

(defn next-player [state]
  (update-in state [:to-play] board/next-player))

(defn do-move [state player index]
  (let [new-state (-> state
                      (update-in [:board] board/make-move-i (marks player) index)
                      next-player)]
    (if-let [winner (or
                     (board/which-winner? (:board new-state))
                     (board/cats-game? (:board new-state)))]
      (toggle-playing new-state)
      new-state)))


;;; #################################################################
;;; Drawing/sizing functions
;;; #################################################################

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
    (dorun
     (map (fn [r mark]
            (g/draw g2d r rect-style)
            (when (not= mark " ")
              (draw-letter
               g2d r mark rect-style)))
          rects
          (:board @game-state)))))


;;; #################################################################
;;; Game Flow handling functions
;;; #################################################################

(defn click->index [e]
  (let [pt (.getPoint e)
        rects (map (fn [r]
                     (.contains r pt))
                   (get-grid-rects (s/to-root e)))]
    (.indexOf rects true)))

(defn process-move [e]
  (when (:playing? @game-state)
    (cond
      (and (= (:to-play @game-state)
              :player)
           (not (nil? e)))
      (do
        (swap! game-state do-move :player (click->index e))
        ;; The AI should always play right after the player
        (recur nil))

      (and (= (:to-play @game-state)
              :ai)
           (nil? e))
      (swap! game-state do-move :ai (-> @game-state
                                        :board
                                        (ai/best-ranked-move marks)
                                        board/xy->index))

      :else nil)))

(defn handle-click [e]
  (if (:playing? @game-state)
    (process-move e)
    (do
      (swap! game-state toggle-playing)
      (process-move nil)))) ; We attempt to make an AI move here in
                            ; case the AI should go first



;;; #################################################################
;;; Basic GUI Setup
;;; #################################################################

(defn show-frame [frame]
  (s/invoke-later
   (-> frame
       s/show!)
   (b/bind game-state
           (b/b-do [_]
               (s/repaint! (get-canvas frame))))
   (s/listen (get-canvas frame)
       :mouse-released handle-click)))

(defn -main [& args]
  (compare-and-set!
   root nil (s/frame :title "Tic-Tac-Toe"
                     :size [600 :by 480]
                     ;; :on-close :exit TODO: remove
                     :content
                     (s/border-panel
                      :center
                      (s/canvas :id :canvas
                                :paint draw-board))))
  (show-frame @root))

;; TODO: remove!!
(use 'seesaw.dev)
