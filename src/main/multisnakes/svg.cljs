(ns multisnakes.svg
  (:require [thi.ng.color.core :as colors]
            [thi.ng.math.core :as math]
            [multisnakes.snake :as snake]))
(def color-palette (repeatedly colors/random-rgb))

(defn board-rect-renderer [board]
  (let  [cell-w (/ 200.0 (:width board))
         cell-h (/ 200.0 (:height board))]
    (fn [[x y] color]
      [:rect {:x (* x cell-w)
              :y (* y cell-h)
              :width cell-w
              :height cell-h
              :fill @(colors/as-css color)}])))
(defn text-renderer [board]
   (let  [cell-w (/ 200.0 (:width board))
          cell-h (/ 200.0 (:height board))]
     (fn [[x y] text color]
       [:text {:x (* x cell-w)
               :y (* y cell-h)
               :fill @(colors/as-css color)}
        text])))

(defn circle-renderer [board]
  (let  [cell-w (/ 200.0 (:width board))
         cell-h (/ 200.0 (:height board))]
    (fn [[x y] radius color]
      [:circle {:style {:transition "all 0.5s ease-in"}
                :cx (+ (* x cell-w) (/ cell-w 2.0))
                :cy (+ (* y cell-h) (/ cell-h 2.0))
                :r (* (min cell-w cell-h) radius)
                :fill @(colors/as-css color)}])))

       

(defn with-color-interpolation [sequence color1 color2]
  (let [len (count sequence)]
    (map-indexed
     #(vector
       (colors/as-css
         (math/mix color1 color2 (/ %1 len)))
       %2)
     sequence)))

(defn svg-board [board]
  (let [text (text-renderer board)
        circ (circle-renderer board)
        rect (board-rect-renderer board)]
    [:svg
     {:viewBox "0 0 200 200"} ;(:height board)}
     (for [[[color1 color2] s]     (map vector (map vec (partition 2 color-palette)) (vals (:snakes board)))
           [color [x y]] (with-color-interpolation (:positions s) color1 color2)]
       ^{:key (str "pos_snake_" (:id s) "_pos_" x "_" y "_" (random-uuid))}
       [rect [x y] color])
     (for [s (vals (:snakes board))
           :let [[x y] (snake/get-head s)]]
       ^{:key (str "label_snake_" (:id s))}
       [text [x y] (:id s) colors/BLACK])
     (let [[x y] (:target-position board)] 
       ^{:key :target-position}
       [circ [x y] 0.5 colors/RED])]))
