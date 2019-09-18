(ns multisnakes.core
  (:require ["express" :as express]
            ["http" :as http]
            ["ws" :as WebSocket]
            ["process" :as process]
            [com.stuartsierra.component :as component :refer [start stop]]
            [multisnakes.snake :as snake :refer [Board Snake]]
            [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [goog.string :as str]
            [cljs.reader :refer [read-string]]))
(defonce server (atom nil))
(defonce contexts (atom {}))

(defn with-timestamp [obj]
  (assoc obj :timestamp (js/Date.)))

(defn add-context! [ctx]
  (swap! contexts assoc (:id ctx) ctx))

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
         (map vector (random-positions w h n))
         (random-names n)))))

(defn create-new-game-context [ws w h snake-count]
  (let [id    (str/getRandomString)
        snakes (random-snakes w h snake-count)
        board (snake/create-board w h snakes)]
    {:id    id
     :board board
     :clients [ws]}))

(defn send-data [ws data]
  (. ws (send (pr-str data))))

(defn send-game-updates [game-id]
  (let [ctx   (get-context game-id)
        board (:board ctx)
        over? (snake/game-over? board)]
    (doall
     (doseq [ws (-> ctx :clients)]
       (send-data ws
                  (with-timestamp
                    (merge
                     (if over? {:game-over? true} {})
                     {:game-id game-id
                      :board   board})))))))

(defn update-board [board])

(defn get-blocked-positions [b snake]
  (let [ss (filter #(not= (:id snake) (:id %)) (vals (:snakes b)))]
    (apply concat (map (comp  :positions) ss))))

(defn start-game-updates [game-id]
  (go-loop [_      (<! (timeout 1000))]
    (send-game-updates game-id)
    (let [ctx     (get-context game-id)
          board   (:board ctx)
          closed? (zero? (count (:clients ctx)))
          over?   (snake/game-over? board)]
      (when-not (or closed? over?)
        (swap! contexts
               update
               game-id
               (fn [ctx]
                 (-> ctx
                     (update :board
                             #(reduce
                               (fn [b s]
                                 (let [dir (-> (snake/target-directions
                                                (get-in b [:snakes s])
                                                (:target-position b)
                                                (get-blocked-positions b (get-in b [:snakes s])))
                                               shuffle
                                               first)]
                                   (snake/play b s dir)))
                               %
                               (keys (get-in % [:snakes])))))))
        (recur (<! (timeout 50)))))))

(defmulti handle-request (fn [ws request] (:type request)))

(defmethod handle-request  :create-game
  [ws {:keys [width height snake-count] :or {width 30 height 30 snake-count 2}}]
  (let [ctx (create-new-game-context ws width height snake-count)]
    (add-context! ctx)
    (start-game-updates (:id ctx))
    ctx))

(defmethod handle-request :join-game [ws {:keys [game-id]}]
  (println "Join game: " game-id)
  (swap! contexts update game-id #(update % :clients conj ws)))

(defn handle-message-fn [ws]
  (fn [message]
    (let [m    (read-string message)
          resp (handle-request ws m)])))

(defn handle-close-fn [ws closed?]
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

