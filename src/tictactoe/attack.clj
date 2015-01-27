(ns tictactoe.attack)

(defn classify [attack my-mark]
  (let [marks (vec (distinct attack))]
    (cond
      (apply = " " marks)              :potential
      (let [c (count marks)]
       (or (= c
              3)
           (and (= c
                   2)
                (not (contains? marks " "))))) :null
      (and (contains? marks " ")
           (contains? marks my-mark))  :shot
      (and (= (count marks)
              2)
           (contains? marks " "))      :threat
      :else                            :unknown)))
