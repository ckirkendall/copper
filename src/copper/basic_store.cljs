(ns copper.basic-store
  (:require
   [copper.core :as core]
   [copper.store :as store]
   [clojure.walk :as walk]))
 

(def base ::base)
(def deps [base ::deps])
(def edit-queue [base ::edits])
(def last-edits [base ::last-edits])


;; ---------------------------------------------------------------------
;; Protocols


(defprotocol IPath
  (-path [this]))

(defn path [obj]
  (-path obj))


;; ---------------------------------------------------------------------
;; Helper Functions

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


;; ---------------------------------------------------------------------
;; Notify Logic

(defn notify-tree [path tree]
  (doseq [[k v]  tree]
    (if (= k ::components)
      (doseq [c v]
        (core/notify c path))
      (notify-tree path v))))


(defn notify-update [path dep-graph]
  (notify-tree
   path
   (reduce (fn [graph seg] 
             (doseq [c (::components graph)]
               (core/notify c path))
             (get graph seg))
           dep-graph
           path)))


(defn notify-updates [paths dep-graph]
  (doseq [path paths]
    (notify-update path dep-graph)))



;; ---------------------------------------------------------------------
;; Simple Store Cursor


(defn read
  ([cursor]
     (core/read cursor [] nil))
  ([cursor path]
     (core/read cursor path nil))
  ([cursor path default]
     (core/read cursor path default)))


(defn transact! [cursor path func]
  (core/transact! cursor path func))

(defn update!
  ([cursor value]
     (transact! cursor [] (fn [] value)))
  ([cursor path value]
     (transact! cursor path (fn [] value))))


(defn -listen! [db component path]
  (let [full-path (concat deps path [::components])]
    (swap! db update-in full-path (fn [components]
                                    (core/add-to #{} components component)))))

(defn -remove-dep! [db component path]
  (let [full-path (concat deps path [::components])]
    (swap! db update-in full-path (fn [components]
                                    (set (filter #(not= % component) components))))))

(defn -read [this path]
  (let [path (or path [])]
    (store/listen! this core/*component* path)
    (get-in @(.-source this) path)))


(defn -transact! [db path func]
  {:pre [(ifn? func)]}
  (swap! db update-in edit-queue (fn [paths]
                                   (core/add-to [] paths {:path path :ufn func}))))

(defn -commit [db]
  (swap! db (fn [state]
              (let [edits (get-in state edit-queue)
                    new-state (reduce (fn [v {:keys [path ufn]}]
                                        (update-in v path ufn))
                                      state
                                      edits)]
                (-> new-state
                    (assoc-in last-edits edits)
                    (assoc-in edit-queue nil))))))

(defn -notify-deps [db]
  (let [edits (get-in @db last-edits)
        dep-graph (get-in @db deps)]
    (notify-updates (map :path edits) dep-graph)))


(defn -dirty? [db]
  (not (nil? (get-in @db edit-queue))))



(extend-type Atom
  store/IStore
  (-store [db]
    (specify (store/Store. db)       
      store/IListen
      (-listen! [_ component [path]]
        (-listen! db component path))
      
      store/IRemoveDep
      (-remove-dep! [_ component [path]]
        (-remove-dep! db component path))
      
      store/IReadable
      (-read [this [path]]
        (-read this path))

      store/ITransact
      (-transact! [_ [path func]]
        (-transact! db path func))

      store/ICommit
      (-commit [_]
        (-commit db))

      store/INotificationSource
      (-notify-deps [_]
        (-notify-deps db))

      store/IDirty
      (-dirty? [_]
        (-dirty? db)))))  


(defn sub-cursor
  "takes a path and existing cursor and returns a cursor
   scoped to that path"
  [cursor & path-parts]
  (let [db cursor
        path (build-path path-parts)]
    (reify
      IPath
      (-path [this] path)
      
      store/IReadable
      (-read [_  [sub-path]]
        (read cursor (concat path sub-path)))

      store/ITransact
      (-transact! [_ [sub-path func]]
        (transact! cursor (concat path sub-path) func))

      IPrintWithWriter 
      (-pr-writer [this writer _]
        (-write writer
                (pr-str "$"
                        {:cursor cursor
                         :path path}))))))



;; ---------------------------------------------------------------------
;; Extending Store to Base Assoc types

(extend-type PersistentHashMap
  store/IStore
  (-store [this]
    (store/-store (atom this))))


(extend-type PersistentArrayMap
  store/IStore
  (-store [this]
    (store/-store (atom this))))


(extend-type PersistentVector
  store/IStore
  (-store [this]
    (store/-store (atom this))))




;; ---------------------------------------------------------------------
;; Associtive Query Cursor

(defn realize-value [form]
  (walk/postwalk
   (fn [node]
     (if (satisfies? store/ITransact node)
       (read node)
       node))
   form))


(deftype ProxyCursor [source]
  store/IReadable
  (-read [_ [path]]
    (let [f (fn temp [src [path-part  & rpaths]]
              (if path-part
                (let [res (get src path-part)]
                  (if (satisfies? store/ITransact res)
                    (read res (vec rpaths))
                    (temp res rpaths)))
                (realize-value src)))]
      (f source path)))

  store/ITransact
  (-transact! [_ [path func]]
    (let [f (fn temp [src [path-part  & rpaths]]
              (if path-part
                (let [res (get src path-part)]
                  (if (satisfies? store/ITransact res)
                    (transact! res (vec rpaths) func)
                    (temp res rpaths)))
                (throw (ex-info "invalid path" {:path path :source source}))))]
      (f source path)))

  ISeqable
  (-seq [this]
    (for [idx (range (count source))]
      (sub-cursor this idx)))

  ICounted
  (-count [_]
    (count source)))


(def built-ins
  {'= =, '== ==, 'not= not=, '!= not=, '< <, '> >, '<= <=, '>= >=, '+ +, '- -,
   '* *, '/ /, 'quot quot, 'rem rem, 'mod mod, 'inc inc, 'dec dec, 'max max, 'min min,
   'zero? zero?, 'pos? pos?, 'neg? neg?, 'even? even?, 'odd? odd?, 'true? true?,
   'false? false?, 'nil? nil?, 'str str, '$ sub-cursor, 'read read
   'list list, 'vector vector})


(defn rset-generator [cursor]
  (let [cnt (count (read cursor))
        res (vec (for [idx (range cnt)]
                   (sub-cursor cursor idx)))]
    res))


(defn value-generator [value] [value])


(defn eval-form [form ctx]
  (walk/postwalk
   (fn [node]
     (cond
      (symbol? node)
      (get ctx node (get built-ins node))
      
      (seq? node)
      (apply (first node) (rest node))

      :else node))
   form))


(defn build-fn [build]
  (fn [ctx]
    (eval-form build ctx)))


(defn where-fn [where]
  (fn [ctx]
    (let [where-vec (eval-form where ctx)]
      (every? identity where-vec))))


(defn from-args [from args]
  (map (fn [sym arg]
         (cond
          (= (first (name sym)) "?")
          [sym (rset-generator arg)]

          (= (first (name sym)) "*")
          [sym (value-generator arg)]

          :else
          (throw (ex-info "query symbos must start with * or ?" {:sym sym}))))
       from
       args))


(defn generate-results [froms where-fn build-fn]
  (let [filt (fn proc [[[sym vals] & rfroms] ctx]
               (if sym
                 (mapcat (fn [v]
                           (proc rfroms (assoc ctx sym v)))
                         vals)
                 (when (where-fn ctx)
                   [(build-fn ctx)])))
        build (vec (filt froms {}))]
    (ProxyCursor. build)))


(defn query [{:keys [build from where]} & args]
  (let [build-fn (build-fn build)
        where-fn (where-fn where)
        froms (from-args from args)]
    (generate-results froms where-fn build-fn)))
