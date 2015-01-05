(ns copper.core
  (:require [copper.store :as store]))

(declare register-store-dep! add-to)

(def render-pending (atom false))

(def ^:dynamic *component*)

;; ---------------------------------------------------------------------
;; Types

(defrecord CopperRoot [stores root-component node render-control])

;; ---------------------------------------------------------------------
;; Base Operations

(defn read [store & params]
  (when *component*
    (register-store-dep! *component* store params))
  (store/-read store params))


(defn transact! [db & args]
  (store/-transact! db args)) 
 

;; ---------------------------------------------------------------------
;; Component Notify Protocols

(defprotocol INotify
  (-notify [obj params]))


(defn notify [obj params]
  (-notify obj params))


;; ---------------------------------------------------------------------
;; Helper Functions

(defn add-to [type container value]
  (if (empty? container)
    (conj type value)
    (conj container value)))


(defn register-store-dep! [component store params]
  (aset component "_paths"
        (add-to #{} (aget component "_paths") [store params])))


(defn cleanup [component]
  (let [paths (aget  component "_paths")]
    (doseq [[db params] paths]
      (apply store/remove-dep! db component params))))


;; ---------------------------------------------------------------------
;; Extending Read and Store to Base Assoc types

(extend-type PersistentHashMap
  store/IReadable
  (-read [this [path default]]
    (get-in this path default)))


(extend-type PersistentArrayMap
  store/IReadable
  (-read [this [path default]]
    (get-in this path default)))


(extend-type PersistentVector
  store/IReadable
  (-read [this [path default]]
    (get-in this path default)))



;; ---------------------------------------------------------------------
;; Copper Component 

(defn react-fns [renderer]
  #js {:shouldComponentUpdate
       (fn [next-props _]
         (this-as this
                  (not= (aget (.-props this) "value")
                        (aget next-props "value"))))
       :render
       (fn []
         (this-as this
                  (binding [*component* this]
                    (renderer
                     (aget (.-props this) "value")
                     (aget (.-props this) "statics")))))
       :unmount
       (fn []
         (this-as this
                  (cleanup (.-props this))))})



(defn component [renderer]
  (let [react-component
        (.createClass
         js/React
         (specify! (react-fns renderer) 
          INotify
          (-notify [this path]
                   (.forceUpdate this))

          IPrintWithWriter 
          (-pr-writer [this writer _]
             (-write writer
                     (pr-str "CopperComponent"
                             {:value (aget (.-props this) "value")
                              :statics (aget (.-props this) "statics")})))))]
    (fn [value static-opts]
      (js/React.createElement react-component
                              #js {:value value :statics static-opts}))))


;; ---------------------------------------------------------------------
;; Primary Render Loop


(defn render-loop [control-atm stores component node]
  (.requestAnimationFrame
           js/window
           (fn []
             (when @control-atm
               (when (some store/dirty? @stores)
                 (doseq [store @stores]
                   (store/commit store)
                   (store/notify-deps store))
                 (.render js/React component node))
               (render-loop control-atm stores component node)))))


(defn create-store [source]
  {:pre [(satisfies? store/IStore source)]}
  (store/store source))


(defn register-store! [{:keys [stores]} store]
  {:pre [(store/is-store? store)]}
  (swap! stores conj store))


(defn root
  ([component node opts] (root #{} component node opts))
  ([stores component node opts]
     (let [render-control (atom true)
           stores-atm (atom #{})
           root-component (component nil opts)]
       (doseq [store stores]
         (register-store! {:stores stores-atm} store))
       (.render js/React root-component node)
       (render-loop render-control stores-atm root-component node)
       (CopperRoot. stores-atm root-component node render-control))))


(defn unmount [{:keys [render-control node]}]
  (js/React.unmountComponentAtNode node)
  (reset! render-control false))

