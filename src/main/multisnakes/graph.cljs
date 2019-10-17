(ns multisnakes.graph
  (:require [multisnakes.snake :as snake :refer [PBoard Board]]
            [astar.core :as astar :refer [route]]))

(defprotocol PGraph
  (graph [this pos])
  (dist [this from to])
  (heuristic-dist [this target-pos])
  (route [this from to]))


(defn get-graph [board pos]
  (let [snakes (:snakes board)
        blocked (set (apply concat (map butlast (map :positions snakes))))]                     
    
    (->> (neighbours pos)
         (filter (comp not blocked)))))

(extend-protocol PGraph
  Board
  (graph [this pos]
    (get-graph this pos))
  
  (dist [this from to]
    (let [[x1 y1] from
          [x2 y2] to]
      (+ (math/abs (- x2 x1)) (math/abs (- y2 y1)))))
  (heuristic-dist [this target-pos] (constantly 0))
  (route [this from to]
    (astar/route (partial graph this) (partial dist this) (partial heuristic-dist this) from to)))
