(ns tictactoe.attack)

(defn classify [attack my-mark other-mark]
  (let [marks (vec (distinct attack))]
    (cond
      (apply = " " marks)                      :potential
      (every? #{" " my-mark} marks)            :shot
      (every? #{" " other-mark} marks)         :threat
      (let [c (count marks)]
        (or (= c
               3)
            (and (= c
                    2)
                 (not (contains? marks " "))))) :null
      :else                                     :unknown)))
