(ns multisnakes.colors
  (:require [thi.ng.color.core :as color]
            [reagent.core :as reagent :refer [atom]]
            [thi.ng.tweeny.core :as tw]
            [thi.ng.math.core :as math]
            [cljs.core.async :as async :refer [go go-loop <! >! chan timeout]]))

(defn with-color-interpolation [sequence color1 color2]
  (let [len (count sequence)]
    (map-indexed
     #(vector
         (color/as-css
           (math/mix color1 color2 (/ %1 len)))
       %2)
     sequence)))

(def COLORS (repeatedly color/random-rgb))
(def GRADIENTS (partition 2 COLORS))

(defn gradient-text
  [text color1 color2]
  (when-let [text (if-not (string? text) (str text) text)]
    [:span.gradient-text
     (for [[color3 [i c]] (with-color-interpolation (map-indexed vector text) color1 color2)]
       [:span {:key (str "char-" i)
               :style {:color @(color/as-css color3)}}
        (str c)])]))

(defn text-animation-kfs [c1 c2 dt]
  [[0 {:v {:color1 @(color/as-rgba c1)
           :color2 @(color/as-rgba c2)}}] ;@(color/as-int32 c2)}}]
   [(/ dt 2) {:v {:color1 @(color/as-rgba c2)}} ;@(color/as-int32 c2)
    :color2 @(color/as-rgba c1)]
   [dt {:v {:color1 @(color/as-rgba c1)}
           :color2 @(color/as-rgba c2)}]]) ;@(color/as-int32 c2)}}]


(defn animated-gradient-text
  [text c1 c2 dt]
  (let [t  (atom 0)
        kf (text-animation-kfs c1 c2 dt)] ;@(color/as-int32 c1)}}]]]
    (go-loop [_ (<! (async/timeout  50))]
      (swap! t (fn [v] (mod (+ 50 v) dt)))
      (recur (<! (async/timeout 50))))
    (fn []
      (let [current-colors          (tw/at @t kf)
            {:keys [color1 color2]} current-colors]
        [gradient-text text (color/rgba color1) (color/rgba color2)]))))

