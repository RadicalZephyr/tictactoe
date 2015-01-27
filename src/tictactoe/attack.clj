(ns tictactoe.attack
  (:require [tictactoe.board :as board
                             :refer [blank]]))

(defn classify [attack my-mark other-mark]
  (let [marks (set attack)]
    (cond
      (= #{blank}            marks) :potential
      (= #{blank my-mark}    marks) :shot
      (= #{blank other-mark} marks) :threat
      (or (= #{blank my-mark other-mark} marks)
          (= #{my-mark other-mark}       marks)) :null
      :else                                      :unknown)))

(defn classify-board [board my-mark other-mark]
  (group-by #(classify (map second %)
                       my-mark other-mark)
            (board/all-indexed-attacks board)))
