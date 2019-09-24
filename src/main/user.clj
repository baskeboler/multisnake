(ns user
  (:require [shadow.user :as shuser]
            [shadow.build :as build]
            [shadow.repl :as repl]
            [shadow.api :as api]
            [shadow.cljs.devtools.api :as devtools]
            [shadow.cljs.util :as cljs-util]
            [shadow.cljs.repl :as cljs-repl]))
; (cljs-util/)
(defn node []
  (devtools/compile :main)
  (devtools/node-repl))


(defn renderer []
  (devtools/compile :renderer)
  (devtools/repl :renderer))
