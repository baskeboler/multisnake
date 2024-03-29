(ns multisnakes.util
  (:require
   [cljs.core.async :as aync :refer [>! <! go go-loop chan]]
   [multisnakes.colors :refer [GRADIENTS gradient-text animated-gradient-text]]
   [multisnakes.snake :as snake]
   [multisnakes.state :as state]))
(defn event-value
  [^js/Event e]
  (let [^js/HTMLInputElement el (.-target e)]
    (.-value el)))

(defn generic-input
  ([type cursor label readonly?]
   [:div.form-group
    [:label label]
    [:input.form-control
     (merge
      {:type        type
       :value       @cursor
       :on-change   (fn [e] (reset! cursor (event-value e)))
       :placeholder label}
      (when readonly?
        {:readOnly   ""}))]])
  ([type cursor label]
   (generic-input type cursor label false)))
(def number-input (partial generic-input :number))
(def text-input (partial generic-input :text))

(defn score-table
  [board]
  [:div.score-table
   [:table.table.table-striped
    [:thead
     [:tr
      [:th "la snake"]
      [:th "puntos"]
      [:th "muerta?"]]]
    [:tbody
     (doall
      (for [[[c1 c2] [id snake]]
            (map vector
                 GRADIENTS
                 (sort-by
                  (fn [[i s]]
                    (* -1
                       (count (:positions s))))
                  @state/snakes))]
        [:tr {:key (str "row-" id)}
         [:td
          [animated-gradient-text id c1 c2 1000]]
         [:td (count (:positions snake))]
         [:td (if (snake/dead?  snake (snake/get-blocked-positions @board snake)) "SI" "NO")]]))]]])

(def fake-names
  (shuffle
   ["pedro" "victor" "maria" "olivia"
    "bonnie" "vicky" "leti" "julia"
    "roberto" "andres" "juan" "pablo"
    "jorge" "rosario" "fernando" "daniela" "daniel"
    "edgardo" "alejandro" "adrian" "virginia"
    "paula" "susana" "gimena" "mariana" "corina"
    "laura" "gaston" "manuel" "francisco"]))
