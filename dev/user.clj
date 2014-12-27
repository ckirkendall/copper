(ns user
  (:require
   [cemerick.piggieback :as piggieback]
   [weasel.repl.websocket :as ws-repl]))

(defn weasel-repl []
  (piggieback/cljs-repl
   :repl-env (ws-repl/repl-env :ip "0.0.0.0" :port 9001)))

(def ws-repl weasel-repl)

(defmacro when-ws-connection
  "Like when-let but for WebSockets. Executes body if a connection to
  the WebSocket address ws-uri can be made."
  [[binding ws-uri :as bindings] & body]
  `(when (~'exists? js/WebSocket)
     (let [ws# (js/WebSocket. ~ws-uri)]
       (set! (.-onopen ws#)
         (fn [e#]
           (.close ws#)
           (let ~(vec bindings)
             ~@body))))))
