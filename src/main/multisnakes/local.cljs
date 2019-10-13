(ns multisnakes.local
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [go go-loop chan timeout <! >!]]
            [multisnakes.state :as state :refer [game-state ws ws-connected? game-type game-type-selected?
                                                 game-over? score game-id canvas board game-opts
                                                 snake-name board-width board-height]]
            [multisnakes.snake :as snake]
            [multisnakes.util :as util :refer [text-input number-input score-table]]
            [multisnakes.colors :refer [COLORS GRADIENTS gradient-text text-animation-kfs animated-gradient-text]]))

(defn create-local-game-btn []
  [:button.btn.btn-sm.btn-primary
   {:on-click (fn [evt]
                (reset! board (snake/create-board  (:width @game-opts) (:height @game-opts) {})))}
   "Create"])

(defn start-local-game []
  (go
    (loop [_ (<! (timeout 10))]
      (let [over? (snake/game-over? @board)]
        (when-not (or false over?)
          (swap! board
                 #(snake/play-round %))
          (recur (<! (timeout 1))))))))

(defn local-new-target-btn []
  [:button.btn.btn-sm.btn-outline-info
   {:on-click #(swap! board
                      (fn [b]
                        (let [new-target (snake/new-target
                                          (:width b)
                                          (:height b)
                                          (:snakes b))]
                          (-> b
                              (assoc :target-position new-target)
                              (update :events conj {:type :new-target
                                                    :message "new target triggered"})))))
    :disabled (nil? @board)}
   "New target"])

(defn local-reset-btn []
  [:button.btn.btn-sm.btn-outline-danger
   {:on-click #(swap! board snake/reset)
    :disabled (nil? @board)}
   "Reset Game"])

(defn local-game-control-panel []
  (let [names       (atom util/fake-names)
        random-name #(let [n (first @names)] (swap! names rest) n)]
    (fn []
      [:div#config-panel.col.collapse
       {:class ""}
       [:div.row>div.col

        [score-table board]
        [:div.inputs
     ;; [text-input game-id "game id" true]
         [:div.form-group
          [:div.input-group
           [:input.form-control
            {:type        :text
             :value       @state/snake-name
             :placeholder "snake name"
             :on-change   #(reset!
                            state/snake-name
                            (-> % .-target .-value))}]
           [:div.input-group-append
            [:button.btn.btn-sm.btn-outline-warning
             {:type :button
              :on-click
              (fn [e]
                (swap! board update-in [:snakes]
                       (fn [snakes]
                         (let [w        @board-width  ; (get-in @board [:width])
                               h        @board-height ; (get-in @board [:height])
                               snake-id (if-not (empty? @state/snake-name) @state/snake-name (random-name))
                               taken    (snake/positions-taken @board)]
                           (-> snakes
                               (assoc snake-id (snake/create-snake [(snake/random-position w h taken)] snake-id)))))))}
             "add snake"]]]]
         [number-input (reagent/cursor game-opts [:width]) "width"]
         [number-input (reagent/cursor game-opts [:height]) "height"]]
        (when @game-over?
          [:div {:style {:color "red"}} "SE KEMÓ TOKIO"])
        [:div.buttons.btn-group
         [create-local-game-btn]
         [:button.btn.btn-sm.btn-secondary
          {:on-click #(start-local-game)}
          "start"]
         [local-reset-btn]
         [local-new-target-btn]]]])))
