(ns tictactoe.attack)

(defn classify [attack my-mark other-mark]
  (let [marks (set attack)]
    (cond
      (= #{" "}            marks) :potential
      (= #{" " my-mark}    marks) :shot
      (= #{" " other-mark} marks) :threat
      (or (= #{" " my-mark other-mark} marks)
          (= #{my-mark other-mark}     marks)) :null
      :else                                    :unknown)))
