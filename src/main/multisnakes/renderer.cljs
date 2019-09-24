(ns multisnakes.renderer
  (:require [reagent.core :as reagent :refer [atom reactify-component]]
            [reagent.impl.template :as rtpl]
            [cljs.core.async :as async :refer [go-loop chan <! >!]]
            [goog.dom :as dom]
            [thi.ng.geom.core :as geom]
            [thi.ng.color.core :as color]
            [multisnakes.snake :as snake]
            [clojure.edn :refer [read-string]]
            [garden.core :as g]
            [goog.string :as str]
            [goog.object :as gobj]))


(set! *warn-on-infer* true)

(defn event-value
  [^js/Event e]
  (let [^js/HTMLInputElement el (.-target e)]
    (.-value el)))

(defonce ws (atom nil))
(defonce ws-connected? (reagent.ratom/make-reaction #(some? @ws)))

(defonce game-over? (atom false))
(defonce score (atom 0))
(defonce game-id (atom nil))
(def canvas (atom nil))
(defonce board (atom nil))
(defonce game-opts
  (atom {:width       25
         :height      25
         :snake-count 0}))

(def canvas-wrapper-styles {:display    :flex
                            :flex-direction :column
                            :align-items :center
                            :position   :relative})

(def canvas-styles {:display  :block
                    :position :relative
                    :width    "40%"
                    :border   "solid 1px black"})

(def ws-endpoint "wss://c3d52b1a.ngrok.io")

(defn draw-position
  [ctx pos cell-w cell-h color]
  (let [[x y] pos]
    (.beginPath ctx)
    (.rect ctx (* x cell-w) (* y cell-h) cell-w cell-h)
    (set! (.-fillStyle ctx) color)
    (.fill ctx)
    (set! (.-lineWidth ctx) 0)))

(defn draw-target
  [ctx pos cell-w cell-h color]
  (let [[x y] pos]
    (.beginPath ctx)
    (.ellipse ctx (+ (* 0.5 cell-w) (* x cell-w)) (+ (* 0.5 cell-h) (* y cell-h)) (* 0.5 cell-w) (* 0.5 cell-h) 0 0 360)
    (set! (.-fillStyle ctx) color)
    (.fill ctx)
    (set! (.-lineWidth ctx) 0)))

(defn with-color-interpolation [sequence color1 color2]
  (let [len (count sequence)]
    (map-indexed #(vector
                   @(color/as-css
                     (thi.ng.math.core/mix
                      color1 color2 (/ %1 len)))
                   %2)
                 sequence)))

(def COLORS (repeatedly color/random-rgb))
(def GRADIENTS (partition 2 COLORS))
(defn generic-input [type cursor label]
  [:div.form-group
   [:label label]
   [:input {:type type
            :value @cursor
            :on-change (fn [e] (reset! cursor (event-value e)))
            :placeholder label}]])
(def number-input (partial generic-input :number))
(def text-input (partial generic-input :text))

(defn draw-board [b w h]
  (let [ctx          (.
                      ;; ^js/HTMLCanvasElement @canvas
                      (dom/getElement "board")
                      (getContext "2d"))
        board-w      (:width b)
        board-h      (:height b)
        cell-w       (/ w board-w)
        cell-h       (/ h board-h)]
    (. ctx (clearRect 0 0 w h))
    (doall
     (doseq [[snake-color s] (map
                              vector
                              GRADIENTS
                              (vals (:snakes b)))
             :let [[color1 color2] snake-color
                   [head-x head-y] (snake/get-head s)
                   color1 (color/rotate-hue color1 (* (count (:positions s)) 0.3))
                   color2 (color/rotate-hue color2 (* (count (:positions s)) -0.3))]]
       (doseq [[c [x y]] (with-color-interpolation
                           (get-in s [:positions])
                           color1
                           color2)]
         (draw-position ctx [x y] cell-w cell-h c))
       (.. ctx (strokeText (:id s) (* cell-w head-x) (* cell-h head-y)))))
    (draw-target ctx (:target-position b)
                   cell-w cell-h "red")))

(defn random-positions [w h n]
  (take n
        (shuffle
         (for [i (range w)
               j (range h)]
           [i j]))))

(defn random-names [n]
  (repeatedly n str/getRandomString))

(defn random-snakes [w h n]
  (into
   {}
   (map
    #(vector (:id %) %)
    (map snake/create-snake
         (map vector
              (random-positions w h n))
         (random-names n)))))

(defn create-new-game-context [ws w h snake-count]
  (let [id    (str/getRandomString)
        snakes (random-snakes w h snake-count)
        board (snake/create-board w h snakes)]
    {:id    id
     :start (js/Date.)
     :board board
     :clients [ws]}))

(defn board-canvas [b w h]
  (reagent/create-class
   {:display-name "board-canvas"
    :component-did-mount
    (fn [this]
      (let [node      (reagent/dom-node this)
            canvas-el (dom/getFirstElementChild node)]
        (reset! canvas canvas-el)
        (draw-board b w h)))
      ;; :component-did-update
      ;; (fn [this]
        ;; (draw-board  b w h))
    :reagent-render
    (fn [b w h]
      [:div {:style canvas-wrapper-styles}
       [:canvas#board {:width w :height h :style canvas-styles}]])}))

(defn handle-message [evt]
  (let [data (read-string {:readers {'multisnakes.snake.Board snake/map->Board
                                     'multisnakes.snake.Snake snake/map->Snake}}
                          (.-data evt))
        b    (:board data)
        id   (:game-id data)]
    (reset! board b)
    (swap! game-opts assoc :game-id id)
    (reset! score (count (get-in b [:snake :positions])))
    (reset! game-id id)
    (reset! game-over? (snake/game-over? b))
    (draw-board  b 500 500)))

(defn game-request [w h snake-count]
  {:type   :create-game
   :width  (int w)
   :height (int h)
   :snake-count (int snake-count)})

(defn new-target-request [game-id]
  {:type :new-target
   :game-id game-id})

(defn add-snake [game-id name]
  {:type :add-snake
   :snake-id name
   :game-id game-id})

(defn remove-snake [game-id snake-id]
  {:type :remove-snake
   :game-id game-id
   :snake-id snake-id})



(defn send-data [ws data]
  (. ws (send (pr-str data))))

(defn open-websocket [url]
  (let [ws (js/WebSocket. url)]
    (. ws (addEventListener "open" #(send-data ws (game-request (:width @game-opts)
                                                                (:height @game-opts)
                                                                (:snake-count @game-opts)))))

    (. ws (addEventListener "message" handle-message))
    ws))
(defn play-local [w h snake-count]
  (let [out (async/chan)]
    (go-loop [board (snake/create-board w h (random-snakes w h (int snake-count)))]
      ;; (draw-board board (:width board) (:height board))
      (>! out board)
      (let [over? (snake/game-over? board)]
        (when-not over?
          (recur
           (reduce
             (fn [b s] 
               (if-not (snake/dead? (get-in b [:snakes s]) (snake/get-blocked-positions b (get-in b [:snakes s])))
                 (let [dir (-> (snake/target-directions
                                (get-in b [:snakes s])
                                (:target-position b)
                                (snake/get-blocked-positions b (get-in b [:snakes s])))
                               shuffle
                               first)]
                   (snake/play b s dir))
                 b))
             board
             (keys (get-in board [:snakes])))))))
    (go-loop [b (<! out)]
      (when-not (nil? b)
        (reset! board b)
        (draw-board b 400 400)
        (<! (async/timeout 50))
        (recur (<! out))))))

(defn start-ws-btn []
  [:button
   ;; {:on-click #(play-local (:width @game-opts) (:height @game-opts) (:snake-count @game-opts))
   {:on-click #(reset! ws (open-websocket ws-endpoint))
    :disabled (not= nil @ws)}
   "Create"])
(defn start-game-btn []
  [:button
   {:on-click #(send-data @ws {:type :start-game
                               :game-id @game-id})}
   "START"])
(defn stop-ws-btn []
  [:button
   {:on-click #(do (. @ws (close))
                   (reset! ws nil))
    :disabled (nil? @ws)}
   "Stop"])

(defn new-target-btn []
  [:button
   {:on-click #(send-data @ws (new-target-request @game-id))
    :disabled (nil? @ws)}
   "New target"])

(defn join-game [url]
  (let [req {:type    :join-game
             :game-id @game-id}
        ws  (js/WebSocket. url)]
    (. ws (addEventListener "open" #(send-data ws req)))
    (. ws (addEventListener "message" handle-message))
    ws))

(defn add-snake [game-id snake-id]
  {:type :add-snake
   :game-id game-id
   :snake-id snake-id})

(defn gradient-text
  [text color1 color2]
  (let [text (if-not (string? text) (str text) text)]
    [:span.gradient-text
     (for [[color3 c] (with-color-interpolation text color1 color2)]
       ^{:key (str "char-" color3)}
       [:span {:style {:color color3}}
        c])]))

(defn score-table
  []
  [:div.score-table
   [:style
    (g/css [:table {:width "80%"}
            [:thead>tr>th {:font-size "1.1em"
                           :text-transform :uppercase}]
            [:tbody
             [:tr
              [:td {:text-align :center}]]]])]
   [:table
    [:thead
     [:tr
      [:th "la snake"]
      [:th "puntos"]
      [:th "muerta?"]]]
    [:tbody
     (for [[[c1 c2] [id snake]] (map vector GRADIENTS (:snakes @board))]
       [:tr {:key (str "row-" id)}
        [:td (gradient-text id c1 c2)]
        [:td (count (:positions snake))]
        [:td (if (:dead? snake) "SI" "NO")]])]]])

(defn ^:export main-component []
  [:div.main-component
   [:style
    (g/css
     [:h1 {:text-transform :uppercase
           :text-align :center
           :font-family :sans-serif
           :margin-bottom "1em"}]
     [:label {:text-transform :uppercase
              :margin-right "2em"}]
     [:div.v-container
      {:display        :flex
       :flex-direction :column
       :align-content  :space-around
       :justify-content  :space-around}]
     [:canvas {:min-width "30em"}]
     [:div.h-container
      {:display        :flex
       :flex-direction :row
       :align-content  :space-around
       :justify-content  :space-around}]
     [:button {:padding "0.6em"
               :text-transform :uppercase
               :border-radius "0.5em"
               :margin "0.5em"
               :border "solid yellow 3px"
               :background-color :white}
      [:&:hover
       {:background-color "yellow"
        :border-color     :black
        :font-weight      600}]])]
   [:h1 "snakes de mierda"]
   [:div.h-container
    [:div.v-container
     [score-table]
     [:div.inputs
      [text-input (reagent/cursor game-opts [:game-id]) "game id"]
      [text-input (reagent/cursor game-opts [:snake-name])  "snake name"]
      [:button {:type :button
                :on-click (fn [e]
                            (send-data @ws (add-snake (:game-id @game-opts) (:snake-name @game-opts))))}
       "add snake"]

      [number-input (reagent/cursor game-opts [:width]) "width"]
      [number-input (reagent/cursor game-opts [:height]) "height"]
      [number-input (reagent/cursor game-opts [:snake-count]) "snake count"]]
     [:div.join
      [:input {:on-change   #(reset! game-id (-> % .-target .-value))
               :value       @game-id
               :type        :text
               :placeholder "Game ID"}]
      [:button {:type     :button
                :on-click #(reset! ws (join-game ws-endpoint))}
       "Join game"]]
     (when @game-over?
       [:div {:style {:color "red"}} "SE KEMÓ TOKIO"])
     [:div.buttons
      [start-ws-btn]
      [start-game-btn]
      [stop-ws-btn]
      [new-target-btn]]]
    [board-canvas @board 500 500]]])

(defn init! []
  (reagent/render
   [main-component]
   (. js/document (getElementById "root"))))

(defn ^:export main []
  (println "renderer process")
  (init!))

