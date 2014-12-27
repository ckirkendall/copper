(ns user
  (:require
   [weasel.repl :as ws-repl])
  (:require-macros
   [user :refer [when-ws-connection]]))

(when-ws-connection [ws-uri "ws://localhost:9001"]
  (ws-repl/connect ws-uri :verbose true))
 
