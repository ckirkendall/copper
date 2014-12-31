(ns copper.core)

(declare register-path add-to)

(def render-pending (atom false))

(def ^:dynamic *component*)

(def base ::base)
(def deps [base ::deps])
(def edit-queue [base ::edits])
(def last-edits [base ::last-edits])

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
;; Notify Logic

(defn notify-tree [path tree]
  (doseq [[k v]  tree]
    (if (= k ::components)
      (doseq [c v]
        (notify c path))
      (notify-tree path v))))


(defn notify-update [path dep-graph]
  (notify-tree
   path
   (reduce (fn [graph seg] 
             (doseq [c (::components graph)]
               (notify c path))
             (get graph seg))
           dep-graph
           path)))


(defn notify-updates [paths dep-graph]
  (doseq [path paths]
    (notify-update path dep-graph)))




;; ---------------------------------------------------------------------
;; Helper Functions


(defn add-to [type container value]
  (if (empty? container)
    (conj type value)
    (conj container value)))


(defn register-path [component store path]
  (aset component "_paths"
        (add-to #{} (aget component "_paths") [store path])))


(defn build-path
  ([path-parts] (build-path [] path-parts))
  ([seed [part & rparts]]
     (if part 
       (let [res (cond
                  (nil? part)
                  seed

                  (satisfies? IPath part)
                  (concat seed (path part))

                  :else
                  (conj seed part))]
         (build-path res rparts))
       seed)))


(defn cleanup [component]
  (let [paths (aget  component "_paths")]
    (doseq [[db path] paths]
      (remove-dep! db component path))))

;; ---------------------------------------------------------------------
;; Simple Store Cursor


(extend-type Atom
  IStore
  (-store [db]
    (specify (Store. db) 
      IListen
      (-listen! [_ component path]
        (let [full-path (concat deps path [::components])]
          (swap! db update-in full-path (fn [components]
                                          (add-to #{} components component)))))
      IRemoveDep
      (-remove-dep! [_ component path]
        (let [full-path (concat deps path [::components])]
          (swap! db update-in full-path (fn [components]
                                          (set (filter #(not= % component) components))))))
      IReadable
      (-read [this path]
        (listen! this *component* path)
        (get-in @db path))

      ITransact
      (-transact! [_ path func]
        (swap! db update-in edit-queue (fn [paths]
                                         (add-to [] paths {:path path :ufn func}))))

      ICommit
      (-commit [_]
        (swap! db (fn [state]
                    (let [edits (get-in state edit-queue)
                          new-state (reduce (fn [v {:keys [path ufn]}]
                                              (update-in v path ufn))
                                            state
                                            edits)]
                      (-> new-state
                          (assoc-in last-edits edits)
                          (assoc-in edit-queue nil))))))

      INotificationSource
      (-notify-deps [_]
        (let [edits (get-in @db last-edits)
              dep-graph (get-in @db deps)]
          (notify-updates (map :path edits) dep-graph)))

      IDirty
      (-dirty? [_]
        (not (nil? (get-in @db edit-queue)))))))


(defn sub-cursor
  "takes a path and existing cursor and returns a cursor
   scoped to that path"
  [cursor & path-parts]
  (let [db cursor
        path (build-path path-parts)]
    (reify
      IPath
      (-path [this] path)
      
      IReadable
      (-read [_  sub-path]
        (read cursor (concat path sub-path)))

      ITransact
      (-transact! [_ sub-path value]
        (transact! cursor (concat path sub-path) value))

      IPrintWithWriter 
      (-pr-writer [this writer _]
        (-write writer
                (pr-str "$"
                        {:cursor cursor
                         :path path}))))))


;; ---------------------------------------------------------------------
;; Extending Read and Store to Base Assoc types

(extend-type PersistentHashMap
  IStore
  (-store [this]
    (-store (atom this)))
  
  IReadable
  (-read [this path]
    (get-in this path)))


(extend-type PersistentArrayMap
  IStore
  (-store [this]
    (-store (atom this)))
  
  IReadable
  (-read [this path]
    (get-in this path)))


(extend-type PersistentVector
  IStore
  (-store [this]
    (-store (atom this)))
  
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

