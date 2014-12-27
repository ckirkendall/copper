(ns copper.core)

(declare register-path add-to)

(def render-pending (atom false))

(def ^:dynamic *component*)

(def base ::base)
(def deps [base ::deps])
(def edit-queue [base ::edits])
(def last-edits [base ::last-edits])


;; ---------------------------------------------------------------------
;; Protocols

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

(defn read [store path]
  (when *component*
    (register-path *component* store path))
  (-read store path))


(defprotocol ITransact
  (-transact! [this path value]))

(defn transact! [db path value]
  (-transact! db path value))


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
;; Simple Store Cursor

(extend-type Atom
  IListen
  (-listen! [db component path]
    (let [full-path (concat deps path [::components])]
      (swap! db update-in full-path (fn [components]
                                      (add-to #{} components component)))))
  IRemoveDep
  (-remove-dep! [db component path]
    (let [full-path (concat deps path [::components])]
        (swap! db update-in full-path (fn [components]
                                        (set (filter #(not= % component) components))))))
  IReadable
  (-read [db path]
    (listen! db *component* path)
    (get-in @db path))

  ITransact
  (-transact! [db path value]
    (swap! db update-in edit-queue (fn [paths]
                                     (add-to [] paths {:path path :value value}))))

  ICommit
  (-commit [db]
    (swap! db (fn [state]
                (let [edits (get-in state edit-queue)
                      new-state (reduce (fn [v {:keys [path value]}]
                                          (assoc-in v path value))
                                    state
                                    edits)]
                  (-> new-state
                      (assoc-in last-edits edits)
                      (assoc-in edit-queue nil))))))

  INotificationSource
  (-notify-deps [db]
    (let [edits (get-in @db last-edits)
          dep-graph (get-in @db deps)]
      (notify-updates (map :path edits) dep-graph)))

  IDirty
  (-dirty? [db]
    (not (nil? (get-in @db edit-queue)))))


(defn sub-cursor
  "takes a path and existing cursor and returns a cursor
   scoped to that path"
  [cursor path]
  (let [db cursor]
    (reify
      IReadable
      (-read [_  sub-path]
        (read cursor (concat path sub-path)))

      ITransact
      (-transact! [_ sub-path value]
        (transact! cursor (concat path sub-path) value)))))


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
        (.createClass js/React
                      (specify! (react-fns renderer) 
                        INotify
                        (-notify [this path]
                           (.forceUpdate this))

                        IPrintWithWriter 
                        (-pr-writer [this writer _]
                           (-write writer
                                   (pr-str "COMPONENT"
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
               (when (some dirty? stores)
                 (doseq [store stores]
                   (commit store)
                   (notify-deps store))
                 (.render js/React component node))
               (render-loop control-atm stores component node)))))


(defn root [stores component node opts]
  (let [render-control (atom true)
        root-component (component nil opts)]
    (.render js/React root-component node)
    (render-loop render-control stores root-component node)
    {:node node
     :stores stores
     :root-component root-component
     :render-control render-control}))


(defn unmount [{:keys [render-control node]}]
  (js/React.unmountComponentAtNode node)
  (reset! render-control false))


