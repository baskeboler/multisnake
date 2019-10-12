(ns multisnakes.colors
  (:require [thi.ng.color.core :as color]
            [thi.ng.tweeny.core :as tw]
            [cljs.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [reagent.core :as reagent :refer [atom]]))

(defn with-color-interpolation [sequence color1 color2]
  (let [len (count sequence)]
    (map-indexed #(vector
                   ;; @(color/as-css
                   (tw/mix
                    color1 color2 (/ %1 len))
                   %2)
                 sequence)))

(def COLORS (repeatedly color/random-rgb))
(def GRADIENTS (partition 2 COLORS))

(defn gradient-text
  [text color1 color2]
  (when-let [text (if-not (string? text) (str text) text)]
    [:span.gradient-text
     (for [[i [color3 c]] (map-indexed vector
                                       (with-color-interpolation text color1 color2))]
       [:span {:key (str "char-" i)
               :style {:color color3}}
        (str c)])]))

(defn text-animation-kfs [c1 c2 dt]
  [[0 {:v {:color1 c1
           :color2 c2}}] ;@(color/as-int32 c2)}}]
   [dt {:v {:color1 c2 ;@(color/as-int32 c2)
            :color2 c1}}]])

(defn animated-gradient-text
  [text c1 c2 dt]
  (let [t  (reagent/atom 0)
        kf (text-animation-kfs c1 c2 dt)] ;@(color/as-int32 c1)}}]]]
    (go-loop [_ (<! (async/timeout  100))]
      (swap! t (fn [v] (mod (+ 100 v) dt)))
      (recur (<! (async/timeout 100))))
    (fn []
      (let [current-colors          (tw/at @t kf)
            {:keys [color1 color2]} current-colors]
        [gradient-text text color1 color2]))))

