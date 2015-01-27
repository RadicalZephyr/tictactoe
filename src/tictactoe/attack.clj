(ns tictactoe.attack
  (:require [tictactoe.board :refer [blank]]))

(defn classify [attack my-mark other-mark]
  (let [marks (set attack)]
    (cond
      (= #{blank}            marks) :potential
      (= #{blank my-mark}    marks) :shot
      (= #{blank other-mark} marks) :threat
      (or (= #{blank my-mark other-mark} marks)
          (= #{my-mark other-mark}       marks)) :null
      :else                                      :unknown)))
