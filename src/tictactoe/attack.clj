(ns tictactoe.attack)

(defn classify [attack my-mark]
  (let [marks (distinct attack)]
    (cond
      (apply = " " marks)              :potential
      (or (= (count marks)
             3)
          (not (contains? marks " "))) :null
      (and (contains? marks " ")
           (contains? marks my-mark))  :shot
      (and (= (count marks)
              2)
           (contains? marks " "))      :threat
      :else                            :unknown)))
