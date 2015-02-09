(ns tictactoe.attack
  (:require [tictactoe.board :as board
                             :refer [blank]]))

(defn classify [attack my-mark other-mark]
  (let [marks (set attack)
        freqs (frequencies attack)]
    (cond
      (= #{blank}            marks)
      :potential
      (= #{blank my-mark}    marks)
      (if (= (freqs board/blank)
             2)
        :shot
        :win)

      (= #{blank other-mark} marks)
      (if (= (freqs board/blank)
             2)
        :threat
        :loss)

      (or (= #{blank my-mark other-mark} marks)
          (= #{my-mark other-mark}       marks))
      :null

      :else :unknown)))

(defn classify-board [board my-mark other-mark]
  (group-by #(classify (map board/mark %)
                       my-mark other-mark)
            (board/all-indexed-attacks board)))

(defn get-space-frequencies [attack-list]
  (frequencies
   (mapcat #(map board/index %)
           attack-list)))

(defn invert-stat-position [[stat space-list]]
  (map (fn [[pos rank]]
         [pos {stat rank}])
       space-list))

(defn rank-move [[pos stats]]
  [pos (+ (* 1000 (get stats :win  0))
          (* 100  (get stats :loss 0))
          (* 10   (get stats :threat 0))
          (* 10   (get stats :shot 0))
          (* 1    (get stats :potential 0))
          (* -1    (get stats :null 0)))])

(defn rank-moves [board my-mark other-mark]
  (->>
   (classify-board board
                   my-mark
                   other-mark)

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
               datum)))
   (map rank-move)))
