(ns multisnakes.renderer
  (:require [reagent.core :as reagent :refer [atom reactify-component]]
            [reagent.impl.template :as rtpl]
            [cljs.core.async :as async :refer [go-loop chan <! >! go timeout]]
            [goog.dom :as dom]
            [thi.ng.geom.core :as geom]
            [thi.ng.color.core :as color]
            [multisnakes.snake :as snake]
            [clojure.edn :refer [read-string]]
            [garden.core :as g]
            [goog.string :as str]
            [goog.object :as gobj]
            [thi.ng.math.core :as math]
            [thi.ng.tweeny.core :as tw]
            [multisnakes.svg :as svg]
            [multisnakes.dom :refer [copy-to-clipboard]]
            ["jexia-sdk-js/browser" :as jexia :refer [UMS JexiaClient]]
            [com.stuartsierra.component :as component]
            [reitit.core :as reitit]
            [reitit.frontend.easy :as rfe]
            [reitit.frontend.controllers :as rfc]
            [multisnakes.config :as config :refer [ws-endpoint]]
            [multisnakes.animations :as a]
            [multisnakes.state :as state :refer [game-state ws ws-connected? game-type game-type-selected?
                                                 game-over? score game-id canvas board game-opts]]
            [multisnakes.util :as util :refer [number-input text-input score-table]]
            [multisnakes.local :refer [local-game-control-panel]]
            [multisnakes.colors :refer [COLORS GRADIENTS gradient-text text-animation-kfs animated-gradient-text]]))
(set! *warn-on-infer* true)

(def canvas-wrapper-styles {:display        :flex
                            :flex-direction :column
                            :align-items    :center
                            :position       :relative})

(def canvas-styles {:display    :block
                    :position   :relative
                    :max-height "80vh"
                    :width      "100%"
                    :border     "solid 1px pink"})

(defn game-type-select-modal []
  [:div.modal.fade
   {:class (when-not @game-type-selected? "show")
    :role :dialog}
   [:div.modal-dialog.modal-dialog-centered>div.modal-content
    [:div.modal-header
     [:h5.modal-title "Select game type"]]
    [:div.modal-body
     [:div.container
      [:div.row>div.col-12>form.form.game-type-content
       [:form-group
        [:label "game type"]
        [:div.btn-group.btn-group-toggle
          ;; {:data-toggle "buttons"}
         (doall
          (for [opt [{:label "Remote" :value :remote}
                     {:label "Local" :value :local}]]
            ^{:key (str "lbl-opt-" (name (:value opt)))}
            [:label.btn.btn-secondary
             {:class (when (= (:value opt) @game-type) "active")}
             [:input (merge
                      {:type :radio
                       :id (str "opt-" (name (:value opt)))
                       ;; :autoComplete :off
                       :on-click #(reset! game-type (:value opt))}
                      (when (= (:value opt) @game-type)
                        {:checked ""}))]
             (:label opt)]))]]]]]
    [:div.modal-footer
     [:button.btn.btn-primary
      "Select"]]]])

(defn draw-position
  [ctx pos cell-w cell-h color]
  (let [[x y] pos]
    (.beginPath ctx)
    (.rect ctx (* x cell-w) (* y cell-h) cell-w cell-h)
    (set! (.-fillStyle ctx) color)
    (set! (.-strokeStyle ctx) color)
    (.fill ctx) ;(.stroke ctx)
    (set! (.-lineWidth ctx) 0)))

(defn explode-target
  [x y color radius time]
  (let [kf [[1 {:v {:pos    [x y]
                    :radius radius
                    :color  (color/as-rgba color)
                    :time   0}}]
            [60 {:v {:pos    [x y]
                     :radius (* 2 radius)
                     :color  (-> color
                                 (color/as-rgba)
                                 (color/adjust-alpha  -1.0)
                                 (color/adjust-saturation -0.5))
                     :time   time}}]]]
    (map #(vector % (tw/at % kf)) (range 30))))

#_(defn draw-target
    [ctx pos cell-w cell-h color]
    (let [[x y] pos]
      (.beginPath ctx)
      (.ellipse ctx (+ (* 0.5 cell-w) (* x cell-w)) (+ (* 0.5 cell-h) (* y cell-h)) (* 0.5 cell-w) (* 0.5 cell-h) 0 0 360)
      (set! (.-fillStyle ctx) color)
      (.fill ctx)
      (set! (.-lineWidth ctx) 0)))
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

(defn handle-message [evt]
  (let [data (read-string {:readers {'multisnakes.snake.Board snake/map->Board
                                     'multisnakes.snake.Snake snake/map->Snake}}
                          (.-data evt))
        b    (:board data)
        id   (:game-id data)]
    (reset! board b)
    (swap! game-opts assoc :game-id id)
    (reset! score (dec
                   (count (get-in b [:snake :positions]))))
    (reset! game-id id)
    (reset! game-over? (snake/game-over? b))))

(defn game-request [w h snake-count]
  {:type   :create-game
   :width  (int w)
   :height (int h)
   :snake-count (int snake-count)})

(defn new-target-request [game-id]
  {:type :new-target
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

(defn start-ws-btn []
  [:button.btn.btn-primary
   {:on-click #(reset! ws (open-websocket ws-endpoint))
    :disabled (not= nil @ws)}
   "Create"])

(defn start-game-btn []
  [:button.btn.btn-secondary
   {:on-click #(send-data @ws {:type :start-game
                               :game-id @game-id})}
   "START"])
(defn stop-ws-btn []
  [:button.btn.btn-warning
   {:on-click #(do (. @ws (close))
                   (reset! ws nil))
    :disabled (nil? @ws)}
   "Stop"])

(defn new-target-btn []
  [:button.btn.btn-outline-info
   {:on-click #(send-data @ws (new-target-request @game-id))
    :disabled (nil? @ws)}
   "New target"])

(defn reset-btn []
  [:button.btn.btn-outline-danger
   {:on-click #(send-data @ws {:type :reset-game :game-id @game-id})
    :disabled (nil? @ws)}
   "Reset Game"])

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

(defn remote-game-control-panel []
  [:div#config-panel.col.collapse
   {:class ""}
   [:div.row>div.col

    [score-table]
    [:div.inputs
     [text-input game-id "game id" true]
     [:div.form-group
      [:div.input-group
       [:input.form-control
        {:type :text
         :value (get @game-opts :snake-name)
         :placeholder "snake name"
         :on-change #(reset!
                      (reagent/cursor game-opts [:snake-name]) (-> % .-target .-value))}]
       [:div.input-group-append
        [:button.btn.btn-outline-warning
         {:type :button
          :on-click
          (fn [e]
            (let [{:keys [game-id snake-name]} @game-opts]
              (send-data @ws (add-snake game-id snake-name))))}
         "add snake"]]]]
     [number-input (reagent/cursor game-opts [:width]) "width"]
     [number-input (reagent/cursor game-opts [:height]) "height"]]
    (when @game-over?
      [:div {:style {:color "red"}} "SE KEMÓ TOKIO"])
    [:div.buttons.btn-group
     [start-ws-btn]
     [start-game-btn]
     [stop-ws-btn]
     [reset-btn]
     [new-target-btn]]]])

(defn events-list []
  [:div
   [:h5 "events"]
   [:ul.ul
    (for [[i e] (map-indexed vector (get-in @board [:events]))]
      ^{:key (str "event-" i)}
      [:li (str "[" (:type e) "] - "  (:message e))])]])

(defn ^:export main-component []
  [:div.container
   [:style
    (g/css
     [:h1 {:text-transform :capitalize
           :font-variant   :small-caps
           :text-align     :center
           :font-weight    900
           :line-height    1.0
           :font-family    :sans-serif
           :margin-bottom  "0.3em"}]
     [:div.show {:display :unset}]
     [:label {:text-transform :uppercase
              :margin-right   "2em"}]
     [:div.v-container
      {:display         :flex
       :flex-direction  :column
       :align-content   :space-around
       :justify-content :space-around}]
     [:div.h-container
      {:display         :flex
       :flex-direction  :row
       :align-content   :space-around
       :justify-content :space-around}]
     [:div.btn-group
       {:margin :unset}
       [:button.btn :a.btn
        {:text-transform :uppercase}]])]
   (when-not @game-type-selected?
     [game-type-select-modal])
   [:h1
    "gusanoloco"]
   [:div.row
    (condp = @game-type
      :remote [remote-game-control-panel]
      :local  [local-game-control-panel]
      [:p "select game type"])

    [:div.col
     [:a.btn.btn-outline-danger.btn-block.mb-2
      {:href          "#config-panel"
       :data-toggle   :collapse
       :role          :button
       :aria-expanded false
       :aria-controls "config-panel"}
      "hide panel"]
     [svg/svg-board @board]
     [:div.row>div.col
      [events-list]]]]])

(def routes
  (reitit/router
   [["/" {:controllers [{:start #(js/console.log "root route start")
                         :stop  #(js/console.log "root route stopped")}]}]
    ["/join/:game-id" {:controllers [{:params (fn [match] (get-in match [:path-params :game-id]))
                                      :start  (fn [gid]
                                                (js/console.log "starting join route for game " gid)
                                                (reset! game-id gid)
                                                (reset! ws (join-game ws-endpoint)))}]}]
    ["/create" {:controllers [{:start (fn []
                                        (reset! ws (open-websocket ws-endpoint)))}]}]]))
(defn init! []
  (rfe/start!
   routes
   (fn [match history]
     (println match)
     (println history)
     (rfc/apply-controllers (get-in match [:data :controllers]) match))
   nil)
  ;; (a/init!)
  (reagent/render
   [main-component]
   (. js/document (getElementById "root"))))

(defn ^:export main []
  (println "renderer process")
  (init!))


