(ns multisnakes.svg
  (:require [thi.ng.color.core :as colors]
            [thi.ng.math.core :as math]
            [multisnakes.snake :as snake]))
(def color-palette (repeatedly colors/random-rgb))
(def gradients (map vec
                    (partition 2 color-palette)))
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
    (fn [[x y] text color1 color2]
      [:text {:x (* x cell-w)
              :y (* y cell-h)
              :style {:font-size "0.5em"}
              :fill @(colors/as-css color1)
              :stroke @(colors/as-css color2)}
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
  ;; (println sequence color1 color2)
  (let [len (count sequence)]
    (map-indexed
     #(vector
       (colors/as-css
        (math/mix color1 color2 (/ %1 len)))
       %2)
     sequence)))

(defn svg-snake [board snake color1 color2]
  (let [text      (text-renderer board)
        rect      (board-rect-renderer board)
        circ      (circle-renderer board)
        dead?     (snake/dead? snake (snake/get-blocked-positions board snake))
        positions (vec
                   (map-indexed vector (:positions snake)))]
    [:g
     (for [[color [i pos]] (with-color-interpolation
                             positions
                             (if-not dead? color1 colors/GRAY)
                             (if-not dead? color2 colors/BLACK))]
       ^{:key (str "snake_" (:id snake) "_pos_" i)}
       [rect pos color])
     [text (snake/get-head snake) (:id snake) color1 color2]]))

(defn svg [children]
  [:svg {:viewBox "0 0 200 200"}
   (doall
    (for [c children]
      c))])

(defn svg-board [board]
  (let [text (text-renderer board)
        circ (circle-renderer board)
        rect (board-rect-renderer board)]
    [:svg
     {:viewBox "0 0 200 200" :style {:border "1px solid red"}} ;(:height board)}
     (doall
      (for [[[c1 c2] s] (map vector
                             gradients
                             (vals (:snakes board)))]
        ^{:key (str "snake-svg-" (:id s))}
         [svg-snake board s c1 c2]))
     (let [[x y] (:target-position board)]
       ^{:key :target-position}
        [circ [x y] 0.5 colors/RED])]))
