(ns multisnakes.dom)

(defn copy-to-clipboard [t]
  (let [e (. js/document (createElement "input"))]
    (set! (.-type e) "text")
    (.. js/document -body (appendChild e))
    (set! (.-value e) t)
    (.select e)
    (.setSelectionRange e 0 99999)
    (. js/document (execCommand "copy"))
    (.. js/document -body (removeChild e))))
