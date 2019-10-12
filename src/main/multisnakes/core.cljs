(ns multisnakes.core
  (:require ["express" :as express]
            ["http" :as http]
            ["ws" :as WebSocket]
            ["process" :as process]
            [com.stuartsierra.component :as component :refer [start stop]]
            [multisnakes.snake :as snake]
            [cljs.core.async :as async :refer [go go-loop <! >! sliding-buffer chan timeout]]
            [goog.string :as str]
            [cljs.reader :refer [read-string]]))

(defonce server (atom nil))
(defonce contexts (atom {}))

(defonce ws-response-chan (chan 50))

(defrecord RagingBonerEvent [snake-id timestamp message type])

(defn create-event [{:keys [snake-id timestamp message type]
                     :as   event-conf
                     :or   {snake-id  nil
                            timestamp (js/Date.)
                            message   "NO MESSAGE"
                            type      :unknown}}]
  (map->RagingBonerEvent event-conf))

(defn async-some [pred input-chan]
  (go-loop []
    (let [msg (<! input-chan)]
      (if (pred msg)
        msg
        (recur)))))

(defn with-timestamp [obj]
  (assoc obj :timestamp (js/Date.)))

(defn add-context! [ctx]
  (swap! contexts assoc (:game-id ctx) ctx))

(defn get-context [id]
  (get @contexts id))

(defn remove-ws [ws]
  (swap!
   contexts
   (fn [contexts]
     (->> (for [[id ctx] contexts]
            [id (update ctx
                        :clients
                        (fn [clients]
                          (filter #(not= ws %) clients)))])
          (into {})))))

(defn create-http-server []
  (let [app    ^js (express)
        server (. ^js http (createServer app))]
    server))

(defn create-websocket-server [^js server]
  (new (. ^js WebSocket -Server) (clj->js {:server server})))

(defn random-names [n]
  (repeatedly n str/getRandomString))

(defn create-new-game-context [ws w h snake-count]
  (let [id     (apply str
                      (take 4 (str/getRandomString)))
        snakes {}
        board  (snake/create-board w h snakes)]
    {:game-id id
     :start   (js/Date.)
     :board   board
     :clients [ws]}))

(defn send-data [ws data]
  (. ws (send (pr-str data))))

(defn send-game-updates [game-id]
  (let [ctx   (get @contexts game-id)
        board (:board ctx)
        over? (snake/game-over? board)]
    (doall
     (doseq [ws (-> ctx :clients)
             :let [d (dissoc ctx :clients)]]
       (send-data ws d)))))

(defn update-board [board])

(defn get-blocked-positions [b snake]
  (let [ss (filter #(not= (:id snake) (:id %)) (vals (:snakes b)))]
    (apply concat (map (comp  :positions) ss))))

(defn start-game-updates [game-id]
  (go
    (loop [_ (<! (timeout 100))]
      (send-game-updates game-id)
      (let [ctx     (get-context game-id)
            board   (:board ctx)
            closed? (zero? (count (:clients ctx)))
            over?   (snake/game-over? board)]
        (when-not (or closed? over?)
          (swap! contexts
                 update-in
                 [game-id :board]
                 #(snake/play-round %))
          (recur (<! (timeout 50))))))))

(defmulti handle-request (fn [ws request]
                           (println "Got request: " request)
                           (:type request)))

(defmethod handle-request  :create-game
  [ws {:keys [width height snake-count] :or {width 30 height 30 snake-count 0}}]
  (let [ctx (create-new-game-context ws width height snake-count)]
    (add-context! ctx)
    (dissoc ctx :clients)))

(defmethod handle-request :start-game
  [ws {:keys [game-id]}]
  (start-game-updates game-id)
  nil)
(defmethod handle-request :add-snake
  [ws {:keys [game-id snake-id]}]
  (swap! contexts update-in [game-id :board :snakes]
         (fn [snakes]
           (let [w     (get-in @contexts [game-id :board :width])
                 h     (get-in @contexts [game-id :board :height])
                 taken (snake/positions-taken (get-in @contexts [game-id :board]))]
             (-> snakes
                 (assoc snake-id
                        (snake/create-snake [(snake/random-position w h taken)] snake-id))))))
  (->
   (get @contexts game-id)
   (dissoc :clients)))
(defmethod handle-request :join-game [ws {:keys [game-id]}]
  (println "Join game: " game-id)
  (swap! contexts update game-id #(update % :clients conj ws))
  nil)
(defmethod handle-request :reset-game [w {:keys [game-id]}]
  (println "Reset game: " game-id)
  (swap! contexts update-in [game-id :board] snake/reset))

(defmethod handle-request :new-target [ws {:keys [game-id]}]
  (swap! contexts update game-id (fn [ctx]
                                   (let [new-target (snake/new-target
                                                     (get-in ctx [:board :width])
                                                     (get-in ctx [:board :height])
                                                     (get-in ctx [:board :snakes]))]
                                     (-> ctx
                                         (assoc-in [:board :target-position] new-target)
                                         (update-in [:board :events] conj
                                                    {:type :new-target
                                                     :message "NO MESSAGE "})))))

  nil)

(defn handle-message-fn [^js ws]
  (fn [message]
    (let [m    (read-string message)
          resp (handle-request ws m)]
      (when resp
        (send-data ws resp)))))

(defn handle-close-fn [^js ws closed?]
  (fn []
    (println "Connection closed")
    (remove-ws ws)
    (reset! closed? true)))

(defn handle-connection [^js ws]
  (let [board   (atom nil)
        closed? (atom false)]
    (. ws (on "message" (handle-message-fn ws)))
    (. ws (on "close" (handle-close-fn ws closed?)))))

(defn init [{:keys [^js server ^js wss port] :as opts}]
  (. wss
     (on "connection" handle-connection))
  (. ^js server (listen port (fn [] (println "Server started on port 8999")))))

(defrecord WebsocketServer [port]
  component/Lifecycle

  (component/start [component]
    (println "Starting Websocket server component")
    (let [server    (create-http-server)
          ws-server (create-websocket-server server)]
      (-> component
          (assoc :http-server (init {:server server
                                     :wss    ws-server
                                     :port   port})))))
  (component/stop [component]
    (println "Stopping websocket server component")
    (. ^js (:http-server component) (close))
    (dissoc component :http-server)))

(defn create-ws-component [{:keys [port] :as opts}]
  (map->WebsocketServer opts))

(defn main [& args]
  (reset! server (create-ws-component {:port 8999}))
  (swap! server component/start)
  (. process (on "beforeExit" #(swap! server component/stop)))
  (. process (on "exit" #(swap! server component/stop))))

