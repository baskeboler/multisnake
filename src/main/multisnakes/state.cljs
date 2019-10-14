(ns multisnakes.state
  (:require [reagent.core :as reagent :refer [atom]]))

(defonce game-state
  (atom {:ws                     nil
         :game-type              nil
         :game-over?             false
         :score                  0
         :game-id                nil
         :canvas                 nil
         :board                  nil
         :tweens                 {}
         :show-select-type-modal true
         :game-opts              {:width       25
                                  :height      25
                                  :snake-count 0
                                  :canvas-w    375
                                  :canvas-h    375}}))

(defonce ws (reagent/cursor game-state [:ws]))

(defonce ws-connected? (reagent.ratom/make-reaction #(some? @ws)))
(defonce game-type (reagent/cursor game-state [:game-type]))
(defonce game-type-selected? (reagent.ratom/make-reaction #(some? @game-type)))
(defonce game-over? (reagent/cursor game-state [:game-over?]))
(defonce score (reagent/cursor game-state [:score]))
(defonce game-id (reagent/cursor game-state [:game-id]))
(def canvas (reagent/cursor game-state [:canvas]))
(defonce board (reagent/cursor game-state [:board]))
(defonce snakes (reagent/cursor board [:snakes]))
(defonce game-opts
  (reagent/cursor game-state [:game-opts]))
(defonce snake-name (reagent/cursor game-opts [:snake-name]))
(defonce board-width (reagent/cursor board [:width]))
(defonce board-height (reagent/cursor board [:height]))

(defonce clock
  (let [callback-added? (atom false)
        callback #(reset! callback-added? false)]
    (reagent.ratom/reaction
     (when-not @callback-added?
       (reset! callback-added? true)
       (reagent.core/next-tick callback))
     (.now js/Date))))

(defonce tweens (reagent/cursor game-state [:tweens]))

(defn get-tween [tween-id]
  (get @tweens tween-id))

(defn add-tween [tween-id tween]
  (swap! tweens assoc tween-id tween))
