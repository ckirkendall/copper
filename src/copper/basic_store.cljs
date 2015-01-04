(ns copper.basic-store
  (:require [copper.core :as core :refer [read transact!]]
            [clojure.walk :as walk]))
 

(def base ::base)
(def deps [base ::deps])
(def edit-queue [base ::edits])
(def last-edits [base ::last-edits])

;; ---------------------------------------------------------------------
;; Helper Functions

(defn build-path
  ([path-parts] (build-path [] path-parts))
  ([seed [part & rparts]]
     (if part 
       (let [res (cond
                  (nil? part)
                  seed

                  (satisfies? core/IPath part)
                  (concat seed (core/path part))

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

(extend-type Atom
  core/IStore
  (-store [db]
    (specify (core/Store. db)       
      core/IListen
      (-listen! [_ component path]
                (let [full-path (concat deps path [::components])]
                  (swap! db update-in full-path (fn [components]
                                                  (core/add-to #{} components component)))))
      core/IRemoveDep
      (-remove-dep! [_ component path]
                    (let [full-path (concat deps path [::components])]
                      (swap! db update-in full-path (fn [components]
                                                      (set (filter #(not= % component) components))))))
      core/IReadable
      (-read [this path]
             (core/listen! this core/*component* path)
             (get-in @db path))

      core/ITransact
      (-transact! [_ path func]
                  (swap! db update-in edit-queue (fn [paths]
                                                   (core/add-to [] paths {:path path :ufn func}))))

      core/ICommit
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

      core/INotificationSource
      (-notify-deps [_]
                    (let [edits (get-in @db last-edits)
                          dep-graph (get-in @db deps)]
                      (notify-updates (map :path edits) dep-graph)))

      core/IDirty
      (-dirty? [_]
               (not (nil? (get-in @db edit-queue)))))))


(defn sub-cursor
  "takes a path and existing cursor and returns a cursor
   scoped to that path"
  [cursor & path-parts]
  (let [db cursor
        path (build-path path-parts)]
    (reify
      core/IPath
      (-path [this] path)
      
      core/IReadable
      (-read [_  sub-path]
        (read cursor (concat path sub-path)))

      core/ITransact
      (-transact! [_ sub-path value]
        (transact! cursor (concat path sub-path) value))

      IPrintWithWriter 
      (-pr-writer [this writer _]
        (-write writer
                (pr-str "$"
                        {:cursor cursor
                         :path path}))))))



;; ---------------------------------------------------------------------
;; Extending Store to Base Assoc types

(extend-type PersistentHashMap
  core/IStore
  (-store [this]
    (core/-store (atom this))))


(extend-type PersistentArrayMap
  core/IStore
  (-store [this]
    (core/-store (atom this))))


(extend-type PersistentVector
  core/IStore
  (-store [this]
    (core/-store (atom this))))


;; ---------------------------------------------------------------------
;; Associtive Query Cursor

(comment
  (query '{:build {:value ($ ?a :id) :display ($ ?a :id)}
           :from [?a ?b *v]
           :where [(= (read ?a [:id]) (read ?b [:id]))
                   (= (read ?b [:tm]) *v)]}
         ($ store :a)
         ($ store :b)
         3))


(defn realize-value [form]
  (walk/postwalk
   (fn [node]
     (if (satisfies? core/ITransact node)
       (read node)
       node))
   form))


(deftype ProxyCursor [source]
  core/IReadable
  (-read [_ path]
    (let [f (fn temp [src [path-part  & rpaths]]
              (if path-part
                (let [res (get src path-part)]
                  (if (satisfies? core/ITransact res)
                    (read res (vec rpaths))
                    (temp res rpaths)))
                (realize-value src)))]
      (f source path)))

  core/ITransact
  (-transact! [_ path func]
    (let [f (fn temp [src [path-part  & rpaths]]
              (if path-part
                (let [res (get src path-part)]
                  (if (satisfies? core/ITransact res)
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
