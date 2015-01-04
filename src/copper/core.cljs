(ns copper.core)

(declare register-path add-to)

(def render-pending (atom false))

(def ^:dynamic *component*)

;; ---------------------------------------------------------------------
;; Types

(defrecord Store [source])

(defrecord CopperRoot [stores root-component node render-control])

;; ---------------------------------------------------------------------
;; Protocols

(defprotocol IStore
  (-store [this]))

(defn store [obj]
  (-store obj))


(defprotocol IPath
  (-path [this]))

(defn path [obj]
  (-path obj))


(defprotocol INotify
  (-notify [this path]))

(defn notify [obj path]
  (-notify obj path))


(defprotocol IListen
  (-listen! [this component path]))

(defn listen! [store component path]
  (-listen! store component path))


(defprotocol IRemoveDep
  (-remove-dep! [this component path]))

(defn remove-dep! [store component path]
  (-remove-dep! store component path))


(defprotocol IReadable
  (-read [this path]))

(defn read
  ([store] (read store [] nil))
  ([store path] (read store path nil))
  ([store path default]
     (when *component*
       (register-path *component* store path))
     (let [res (-read store path)]
       (if res res default))))


(defprotocol ITransact
  (-transact! [this path f]))

(defn transact!
  ([db f]
     (-transact! db [] f))
  ([db path f]
     (-transact! db path f))) 

(defn update!
  ([db value]
     (-transact! db [] (fn [] value)))
  ([db path value]
     (-transact! db path (fn [] value))))


(defprotocol ICommit
  (-commit [this]))

(defn commit [obj] 
  (-commit obj))


(defprotocol INotificationSource
  (-notify-deps [this]))

(defn notify-deps [obj]
  (-notify-deps obj))


(defprotocol IDirty
  (-dirty? [this]))

(defn dirty? [obj]
  (-dirty? obj))
 

;; ---------------------------------------------------------------------
;; Helper Functions


(defn add-to [type container value]
  (if (empty? container)
    (conj type value)
    (conj container value)))


(defn register-path [component store path]
  (aset component "_paths"
        (add-to #{} (aget component "_paths") [store path])))


(defn cleanup [component]
  (let [paths (aget  component "_paths")]
    (doseq [[db path] paths]
      (remove-dep! db component path))))

;; ---------------------------------------------------------------------
;; Extending Read and Store to Base Assoc types

(extend-type PersistentHashMap
  IReadable
  (-read [this path]
    (get-in this path)))


(extend-type PersistentArrayMap
  IReadable
  (-read [this path]
    (get-in this path)))


(extend-type PersistentVector
  IReadable
  (-read [this path]
    (get-in this path)))



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
               (when (some dirty? @stores)
                 (doseq [store @stores]
                   (commit store)
                   (notify-deps store))
                 (.render js/React component node))
               (render-loop control-atm stores component node)))))




(defn create-store [source]
  {:pre [(satisfies? IStore source)]}
  (store source))


(defn register-store! [{:keys [stores]} store]
  {:pre [(instance? Store store)
         (satisfies? IListen store)
         (satisfies? IReadable store)
         (satisfies? ICommit store)
         (satisfies? INotificationSource store)
         (satisfies? IDirty store)
         (satisfies? ITransact store)
         (satisfies? IRemoveDep store)]}
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

