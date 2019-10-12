(ns multisnakes.jexia
  (:require [reagent.core :as r :refer [atom]]
            [cljs.core.async :as async :refer [go go-loop chan >! <!]]
            ["jexia-sdk-js/browser" :as jexia :refer [UMSModule jexiaClient dataOperations realTime]]))

(def jexia-client jexiaClient)

(def ums (UMSModule.))
(def rt (realTime))
(def dops (dataOperations))

(def jexia-credentials
  {:projectID "6824d1fb-97d4-4607-bdd4-bd890ac81db0"
   :key       "572010dd-e8fb-4f18-9499-1f8028089d57"
   :secret    "CHbsDLosl7+QjbUeR/N8Ju4AwdPI0M1pMc0cNCkWuByisjZowwSVW5FShFZyPBVxOJ1iujZx/T7olnP7yViyjg=="
   :default   true})

(defn init-jexia
  "Initialize Jexia SDK"
  [creds]
  (let [res      (.init (jexia-client.) (clj->js creds) ums dops rt)
        res-chan (chan)]
     res
     (when-not (nil? res)
       (do
         (.then res #(go
                       (async/>! res-chan %)
                       (async/close! res-chan)))))
     res-chan))
        ;; (async/>! res-chan res)
        ;; (recur (async/<! (async/timeout 100)))))))

(defrecord JexiaService [credentials client]
  component/Lifecycle
  (start [component]
    (println "starting Jexia Service")
    component)
  (stop [component]
    (println "Stopping component")
    (.terminate client)
    component))
(defn create-jexia-service [{:keys [client creds] :as opts}]
  (map->JexiaService opts))

(defn signin
  [^js client {:keys [username password]}]
  (.signIn ums (clj->js {:email    username
                         :password password
                         :defaut   false
                         :alias    "Baskeboler"})))
  
