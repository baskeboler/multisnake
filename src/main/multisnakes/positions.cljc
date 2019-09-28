(ns multisnakes.positions
  (:require [thi.ng.math.core :as math]))
(def directions [:up :right :down :left])

(def direction-deltas {:up    [0 -1]
                       :down  [0 1]
                       :right [1 0]
                       :left  [-1 0]})

(defn add-positions [pos1 pos2]
  (apply ->Position (mapv + (->vec pos1) (->vec pos2))))

(defn distance* [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2)))))

(defprotocol PPosition
  (distance [this other-position])
  (move [this direction steps] [this direction])
  (->vec [this])
  (get-x [this])
  (get-y [this]))

(defrecord Position [x y]
  PPosition
  (get-x [this] (:x this))
  (get-y [this] (:y this))
  (->vec [this] [x y])
  (distance [this other] (distance* (->vec this) (->vec other)))
  (move
    [this direction steps]
    (loop [s   steps
           pos this]
      (if (zero? s)
        pos
        (recur (dec s) (add-positions this (direction-deltas direction))))))
  (move [this direction]
    (move this direction 1)))

(extend-protocol PPosition
  PersistentVector
  (get-x [this] (first this))
  (get-y [this] (second this))
  (->vec [this] this)
  (distance [this other] (distance* this other))
  (move [this direction steps]))
(defn create-position
  ([^number x ^number y]
   (->Position x y))
  ([x]
   (create-position x x))
  ([] (create-position 0)))
