(ns multisnakes.login
  (:require [[reagent.core :as r :refer [atom]]]))

(defn login-formm [{:keys [username password on-click] :as opts}]
  [:div.login-form
   [:div.form-group
    [:label "username"]
    [:div.input-group
     [:div.input-group-preppend
      [:i.fas.fa-fw.fa-user]]
     [:input.form-control
      {:type        :text
       :placeholder "user name"
       :value       @username
       :on-change   (reset! username (-> % .-target .-value))}]]]
   [:div.form-group
    [:input.form-control
     {:type      :password
      :value     @password
      :on-change #(reset! password (-> % .-target .-value))}]]
   [:footer
    [:button.btn.btn-primary
     {:type :button
      :on-click on-click}
     "login"]]])
