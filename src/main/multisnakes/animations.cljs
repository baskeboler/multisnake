(ns multisnakes.animations
  (:require [reagent.core :as r :refer [atom]]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.tweeny.core :as tw]
            [thi.ng.color.core :as c]
            [thi.ng.color.presets.brewer :as brewer]
            [cljs.core.async :as async :refer [chan sliding-buffer <! >! go go-loop]]
            [multisnakes.state :as state :refer [clock]]))

(defonce animation-pipeline (atom []))

(defn vec->css-color [[r g b a]]
  (-> (c/rgba r g b a)
      c/as-css))

(extend-protocol c/ICSSConvert
  PersistentVector
  (as-css [this]
    (-> this
        (c/rgba)
        (c/as-css)))
  nil
  (as-css [this] (c/rgba 0 0 0 0)))

(extend-protocol c/IRGBConvert
  PersistentVector
  (as-rgba [this] this)
  nil)
(extend-protocol svg/ISVGConvert
  PersistentVector
  (as-svg [this _] this))

(defprotocol PAnimation
  (next-frame [this])
  (is-complete? [this]))

(defrecord AnimationFrames [frames]
  PAnimation
  (next-frame [this]
    (-> this
        (update :frames rest)))
  (is-complete? [this]
    (nil? (-> this
              :frames)))
  svg/ISVGConvert
  (svg/as-svg [this opts]
    (svg/as-svg (first (:frames this)) opts)))

(extend-protocol PAnimation
  PersistentVector
  (next-frame [this] (rest this))
  (is-complete? [this] (or (nil? this) (empty? this))))
(defrecord SVGTarget [x y r color]
  svg/ISVGConvert
  (as-svg [this opts]
    (svg/circle [(:x this) (:y this)] (:r this) {:fill @(c/as-css (:color this))})))

(defn enqueue-animation [anim]
  (swap! animation-pipeline conj anim))

(defn exploding-target-animation [{:keys [x y r color] :as opts} frame-count]
  (let [kfs [[0 {:v {:x     x
                     :y     y
                     :r     r
                     :color @(c/as-rgba color)}}]
             [frame-count {:v {:x     x
                               :y     y
                               :r     (* r 2.0)
                               :color (-> color
                                          (c/as-rgba)
                                          (c/adjust-alpha -1.0)
                                          ;; (c/as-css)
                                          deref)}}]]]
    (map (comp
          map->SVGTarget
          #(tw/at % kfs))
         (range frame-count))))

(def current-fx-transducer (comp (map first)
                                 (map svg/as-svg)))

(def next-frame-transducer (comp (map next-frame)
                                 (filter (comp is-complete? not))))

(defn init! []
  (println  "starting animation loop")
  (go-loop [_ (<! (async/timeout 500))]
    (swap! animation-pipeline
           #(sequence next-frame-transducer %))
    (recur (<! (async/timeout 500))))
  (println "animation loop started")

  (go-loop [_ (<! (async/timeout 5000))]
    (let [t (exploding-target-animation
             {:x     (rand-int 20)
              :y     (rand-int 20)
              :r     64
              :color (c/rgba 1 0 0 1)}
             25)]
      (enqueue-animation (vec t))
      (recur (<! (async/timeout 200))))))

(defn ^:export effects-component []

  (into [:g]
        (for [[i some-fx] (vec
                           (map-indexed
                            vector
                            (into [] current-fx-transducer
                                  @animation-pipeline)))]
          ^{:key (str "fx-frame-" i)}
          [some-fx])))

(defn now [] (.now js/Date))

(defn tween
  [dt start-value end-value]
  (let [start-time (now)
        done?      (atom false)
        kf         [[0 {:v start-value :f (tw/mix-exp 0.5)}]
                    [dt {:v end-value :f (tw/mix-exp 1.5)}]]]
    (reagent.ratom/reaction
     (if @done?
       end-value
       (let [elapsed (- @clock start-time)]
         (if (> elapsed dt)
           (do (reset! done? true) end-value)
           (tw/at elapsed kf)))))))

(defn tween2
  [& {:keys [dt start-value end-value delay]
      :or   {dt          1.0
             start-value 0.0
             end-value   1.0
             delay       0.0}
      :as opts}]
  (println dt delay start-value end-value)
  (let [start-time (now)
        done?      (atom false)
        kf         [[0 {:v start-value :f (tw/mix-exp 0.5)}]
                    [delay {:v start-value :f (tw/mix-exp 0.5)}]
                    [(+ delay dt) {:v end-value :f (tw/mix-exp 0.5)}]]]
    (reagent.ratom/reaction
     (if @done?
       end-value
       (let [elapsed (- @clock start-time)]
         (if (> elapsed (+ dt delay))
           (do
             (reset! done? true)
             end-value)
           (tw/at elapsed kf)))))))
