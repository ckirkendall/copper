(ns copper.datascript-store
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]
    [copper.core :as core]
    [copper.store :as store]
    [copper.basic-store :as bs]
    [datascript.core :as dc]
    [datascript :as ds]))


(defn listen-for-pattern! [deps-store [e a v t]]
  (case [(when e :+) (when a :+) (when v :+)]
   [:+  nil nil]
   (bs/-listen! deps-store core/*component* [:ea e])

   [nil :+  nil]
   (bs/-listen! deps-store core/*component* [:av a])

   [:+  :+  nil]
   (bs/-listen! deps-store core/*component* [:ea e a])

   [nil :+  :+]
   (bs/-listen! deps-store core/*component* [:av a v])

   [:+  :+  :+]
   (bs/-listen! deps-store core/*component* [:ea e a])))



(deftype DBWrapper [db deps-store]
  Object
  (toString [this]
    (pr-str* this))

  dc/IDB
  (-schema [_] (dc/-schema db))
  (-refs   [_] (dc/-refs db))

  dc/ISearch
  (-search [_ pattern]
    (listen-for-pattern! deps-store pattern)
    (dc/-search db pattern))

  dc/IIndexAccess
  (-datoms [_ index cs]
    (dc/-datoms db index cs))

  (-seek-datoms [_ index cs]
    (dc/-seek-datoms db index cs))

  (-index-range [_ attr start end]
    (dc/-index-range db attr start end))

  IHash
  (-hash [_] (hash db))

  IEquiv
  (-equiv [_ other]
    (= db other))

  IAssociative
  (-contains-key? [_ k]
    (contains? db k))
  (-assoc [_ k sub-value]
    (DBWrapper. (assoc db k sub-value) deps-store))

  IMap
  (-dissoc [_ k]
    (DBWrapper. (dissoc db k) deps-store))

  ILookup
  (-lookup [_ k]
    (get db k))
  (-lookup [_ k not-found]
    (get db k not-found))

  IPrintWithWriter
  (-pr-writer [this writer _]
    (-write writer (pr-str "Wrapper:" [db ds-store]))))


(defn -queue-transaction [deps-store args]
  (swap! deps-store
         update-in
         bs/edit-queue
         (fn [edits]
           (core/add-to [] edits args))))


(defrecord DSStore [db deps-store]
  store/IListen
  (-listen! [_ component [path]]
    (bs/-listen! deps-store component path))

  store/IRemoveDep
  (-remove-dep! [_ component [path]]
    (bs/-remove-dep! deps-store component path))

  store/IReadable
  (-read [this [path]]
    (throw "read not implemented please use query"))

  store/ITransact
  (-transact! [_ [args]]
    (-queue-transaction deps-store args))

  store/ICommit
  (-commit [_]
    (let [edits (get-in @deps-store bs/edit-queue)]
      (doseq [edit edits]
        (ds/transact! db edit))
      (swap! deps-store (fn [store]
                         (-> store
                             (assoc-in bs/edit-queue nil)
                             (assoc-in bs/last-edits edits))))))

  store/INotificationSource
  (-notify-deps [_]
    (let [last-edits (get-in @deps-store bs/last-edits)
          dep-graph (get-in @deps-store bs/deps)]
      (doseq [edit last-edits]
        (doseq [[f e a v1 v2] edit]
          (bs/notify-updates [[:ea e a]] dep-graph)
          (bs/notify-updates [[:av a v1]] dep-graph)))))

  store/IDirty
  (-dirty? [_]
    (not (nil? (get-in @deps-store bs/edit-queue))))

  IDeref
  (-deref [this] @db))


(defn create-store [& [schema]]
  (let [deps-store (atom {})]
    (DSStore.
     (atom (DBWrapper.
            (ds/empty-db schema)
            deps-store)
           :meta { :listeners  (atom {}) })
     deps-store)))


(def q ds/q)
