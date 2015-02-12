(ns tictactoe.gui
  (:require [tictactoe.ai :as ai]
            [tictactoe.board :as board]
            [seesaw.core :as s]
            [seesaw.bind :as b]
            [seesaw.font :as font]
            [seesaw.graphics :as g]))

(def root (atom nil))

(def game-state (atom {:board board/empty-board
                       :playing? true
                       :to-play :player
                       :marks {:ai     "o"
                               :player "x"}
                       :plays-first :player}))

(defn start-thread [fn]
  (.start (Thread. fn)))

;;; #################################################################
;;; Game State Manipulation Functions
;;; #################################################################

(defn get-marks []
  (:marks @game-state))

(defn reset-board! []
  (swap! game-state (fn [state]
                      (-> state
                          (assoc :board board/empty-board)
                          (assoc :playing? true)
                          (assoc :to-play (:plays-first state))))))

(defn set-order! [first]
  (swap! game-state (fn [state]
                      (-> state
                          (assoc :plays-first first)
                          (assoc :marks {first "x"
                                         (board/next-player first) "o"})))))

(defn toggle-playing [state]
  (update-in state [:playing?] not))

(defn next-player [state]
  (update-in state [:to-play] board/next-player))

(defn do-move [state player index]
  (-> state
      (update-in [:board] board/make-move-i
                 ((get-marks) player) index)
      next-player))

(defn check-end-game [state]
  (if-let [winner (or
                   (board/which-winner? (:board state))
                   (board/cats-game? (:board state)))]
    (toggle-playing state)
    state))

(defn try-move [state player index]
  (if (board/valid-move-i? (:board state) index)
    (-> state
        (do-move player index)
        check-end-game)
    state))


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

(defn draw-x [g2d tl tr bl br style]
  (do
    (g/draw g2d (g/polygon tl br)
            style)
    (g/draw g2d (g/polygon tr bl)
            style)))

(defn draw-o [g2d [x y] width height style]
  (g/draw g2d (g/ellipse x y width height)
          style))

(defn get-inset-coords [rect]
  (let [min-x (.getMinX rect)
        min-y (.getMinY rect)
        max-x (.getMaxX rect)
        max-y (.getMaxY rect)
        width (.getWidth rect)
        height (.getHeight rect)
        inset-w (* 0.1 width)
        inset-h (* 0.1 height)]
    {:tl [(+ inset-w min-x)
          (+ inset-h min-y)]

     :tr [(- max-x inset-w)
          (+ inset-h min-y)]

     :bl [(+ inset-w min-x)
          (- max-y inset-h)]

     :br [(- max-x inset-w)
          (- max-y inset-h)]

     :width (* 0.8 width)
     :height (* 0.8 height)}))

(defn draw-letter [g2d rect letter style]
  (let [{:keys [tl tr bl br width height]} (get-inset-coords rect)]
    (case letter
      "x" (draw-x g2d tl tr bl br style)

      "o" (draw-o g2d tl width height style))))

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
                   (get-grid-rects (s/to-root e)))
        index (.indexOf rects true)]
    (when (not= index -1)
      index)))

(defn try-ai-move []
  (when (and
         (:playing? @game-state)
         (= (:to-play @game-state)
            :ai))
    (swap! game-state
           try-move :ai (-> @game-state
                            :board
                            (ai/best-minimax-move (get-marks))
                            board/xy->index))))

(defn try-player-move [e]
  (when (and
         (:playing? @game-state)
         (= (:to-play @game-state)
            :player))
    (when-let [index (click->index e)]
      (swap! game-state try-move :player index)
      ;; The AI should always play right after the player
      (start-thread try-ai-move))))

(declare show-choose-player)

(defn handle-click [e]
  (if (:playing? @game-state)
    (try-player-move e)
    (show-choose-player (s/to-root e))))


;;; #################################################################
;;; Basic GUI Setup
;;; #################################################################

(defn show-board [root]
  (let [canvas (s/canvas :id :canvas
                         :paint draw-board)]
    (s/config!
     root
     :content (s/border-panel
               :center canvas))
    (b/bind game-state
            (b/b-do [_]
                (s/repaint! canvas)))
    (s/listen canvas
        :mouse-released handle-click)))

(defn start-game [player e]
  (set-order! player)
  (reset-board!)
  (show-board (s/to-root e))
  ;; We attempt to make an AI move here in case the AI should go first
  (start-thread try-ai-move))

(defn vertically-centered [num-fills & items]
  (let [fills (repeat num-fills :fill-v)]
    (s/vertical-panel :items `[~@fills ~@items ~@fills])))

(defn horizontally-centered [num-fills & items]
  (let [fills (repeat num-fills :fill-h)]
    (s/horizontal-panel :items `[~@fills ~@items ~@fills])))

(defn show-choose-player [root & winner]
  (let [[w h] (get-size root)]
    (s/config!
     root
     :content
     (vertically-centered 1
         (horizontally-centered 1
             (s/label :text "Who should play first?"
                      :font (font/font :name :serif
                                       :size 32)))
       [:fill-v (* 0.1 h)]
       (horizontally-centered 2
           (s/action :name "Human"
                     :handler (partial start-game :player))
         :fill-h
         (s/action :name "Computer"
                   :handler (partial start-game :ai)))))))

(defn show-frame [frame]
  (s/invoke-later
   (-> frame
       show-choose-player
       s/show!)))

(defn -main [& args]
  (compare-and-set!
   root nil (s/frame :title "Tic-Tac-Toe"
                     :size [600 :by 480]
                     ;; :on-close :exit TODO: remove
                     ))
  (show-frame @root))

;; TODO: remove!!
(use 'seesaw.dev)
