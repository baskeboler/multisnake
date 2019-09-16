(ns multisnakes.renderer
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async]
            [goog.dom :as dom]
            [thi.ng.geom.core :as geom]
            [thi.ng.color.core :as color]
            [multisnakes.snake :as snake]
            [clojure.edn :refer [read-string]]))
            ;; [cljs.reader :refer [read-string]]))

(defonce ws (atom nil))
(defonce game-over? (atom false))
(defonce score (atom 0))
(defonce game-id (atom nil))

(defonce board (atom nil))
(defonce dimensions (atom {:width 30 :height 30}))

(defn draw-position
  [ctx pos cell-w cell-h color]
  (let [[x y] pos]
    (.beginPath ctx)
    (.rect ctx (* x cell-w) (* y cell-h) cell-w cell-h)
    (set! (.-fillStyle ctx) color)
    (.fill ctx)))
    ;; (set! (.-lineWidth ctx) 0.5)
    ;; (set! (.-strokeStyle ctx) "gray")))
    ;; (.stroke ctx)))

(defn draw-board [b w h]
  (let [ctx (. (dom/getElement "board") (getContext "2d"))
        board-w      (:width b)
        board-h      (:height b)
        cell-w       (/ w board-w)
        cell-h       (/ h board-h)
        snake-length (count (get-in b [:snake :positions]))]
    (. ctx (clearRect 0 0 w h))
    (doall
     (doseq [[c   [x y]] (mapv vector
                               (for [i    (range snake-length)
                                     :let [q (/ i snake-length)
                                           red (color/rgba 1.0 1.0 0 1.0)]]
                                 @(color/as-css (thi.ng.math.core/mix red (color/rgba 0 1.0 0 1.0) q)))
                               (get-in b [:snake :positions]))]
       (draw-position ctx [x y] cell-w cell-h c)))
    (draw-position ctx (:target-position b)
                   cell-w cell-h "red")))

(defn board-canvas [b w h]
  (reagent/create-class
   {:display-name "board-canvas"
    :component-did-mount
    (fn [this]
      (draw-board b w h))
    :reagent-render
    (fn [b w h]
      [:canvas#board {:width w :height h :style {:border "solid 1px black"}}])}))

(defn handle-message [evt]
  (let [data (read-string {:readers {'multisnakes.snake.Board snake/map->Board
                                     'multisnakes.snake.Snake snake/map->Snake}}
                          (.-data evt))
        b    (:board data)
        id   (:game-id data)]
    (reset! board b)
    (reset! score (count (get-in b [:snake :positions])))
    (reset! game-id id)
    (reset! game-over? (snake/game-over? b))
    (draw-board b 500 500)))

(defn game-request [w h]
  {:type   :create-game
   :width  w
   :height h})

(defn send-data [ws data]
  (. ws (send (pr-str data))))

(defn open-websocket [url]
  (let [ws (js/WebSocket. url)]
    (. ws (addEventListener "open" #(send-data ws (game-request (:width @dimensions)
                                                                (:height @dimensions)))))

    (. ws (addEventListener "message" handle-message))
    ws))

(defn start-ws-btn []
  [:button
   {:on-click #(reset! ws (open-websocket "ws://localhost:8999"))
    :disabled (not= nil @ws)}
   "Start"])

(defn stop-ws-btn []
  [:button
   {:on-click #(do (. @ws (close))
                   (reset! ws nil))
    :disabled (nil? @ws)}
   "Stop"])

(defn join-game [url]
  (let [req {:type :join-game
             :game-id @game-id}
        ws (js/WebSocket. url)]
    (. ws (addEventListener "open" #(send-data ws req)))
                            ;; #(. ws (send (js/JSON.stringify (clj->js req))))))
    (. ws (addEventListener "message" handle-message))
    ws))

(defn main-component []
  [:div.main-component
   [:h1 "Main component"]
   [:div
    (str "Score: " @score)]
   (when @game-over?
     [:div {:style {:color "red"}} "GAME OVER"])
   [:div.buttons
    [start-ws-btn] [stop-ws-btn]]

   [:div.inputs
    [:input {:on-change   #(swap! dimensions assoc :width (-> % .-target .-value))
             :type        :number
             :placeholder "width"
             :value       (-> @dimensions :width)}]
    [:input {:on-change   #(swap! dimensions assoc :height (-> % .-target .-value))
             :type        :number
             :placeholder "height"
             :value       (-> @dimensions :height)}]]
   [:div.join
    [:input {:on-change #(reset! game-id (-> % .-target .-value))
             :value @game-id
             :type :text
             :placeholder "Game ID"}]
    [:button {:type :button
              :on-click #(reset! ws (join-game "ws://localhost:8999"))}
     "Join game"]]
   [board-canvas @board 500 500]])

(defn init! []
  (reagent/render
   [main-component]
   (. js/document (getElementById "root"))))

(defn main []
  (println "renderer process")
  (init!))
