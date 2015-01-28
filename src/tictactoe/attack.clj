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

(defn get-space-frequencies [attack-list]
  (frequencies
   (mapcat #(map first %)
           attack-list)))

(defn invert-stat-position [[stat space-list]]
  (map (fn [[pos rank]]
         [pos {stat rank}])
       space-list))

(defn duplicate-space-pos [attack]
  (let [space (some (fn [[pos val :as square]]
                      (when (= val board/blank)
                        square))
                    attack)]
    (conj attack space)))

(defn weight-attack [attack my-mark other-mark]
  (let [marks (frequencies (map second attack))]
    (cond
      ;; We want to weight the remaining space in a double threat/shot
      ;; more heavily
      (or (= #{" " 1 my-mark 2}
             marks)
          (= #{" " 1 other-mark 2}
             marks))
      (duplicate-space-pos attack))
    :else attack))

(defn rank-spaces [board my-mark other-mark]
  (->>
   (classify-board board
                   my-mark
                   other-mark)

   ;; We can mangle the attacks now because classification is done
   (map (fn [[k v]]
          [k (map #(weight-attack %
                                  my-mark
                                  other-mark)
                  v)]))

   ;; Now count up how many times each space appears in each category
   (map (fn [[k v]]
          [k (get-space-frequencies v)]))
   (map invert-stat-position)
   ;; Make them all back into dictionaries separately so we don't lose data
   (map #(into {} %))
   ;; Then merge them together
   (apply merge-with merge)
   ;; Finally, filter out all spaces that aren't legal moves
   (filter (fn [[pos info :as datum]]
             (when (board/valid-move? board pos)
               datum)))))
