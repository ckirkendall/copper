(ns copper.store)

(defrecord Store [source])


(defprotocol IStore
  (-store [this]))

(defn store [this]
  (-store this))


(defprotocol IRemoveDep
  (-remove-dep! [this component params]))

(defn remove-dep! [this component & params]
  (-remove-dep! this component params))


(defprotocol IListen
  (-listen! [this component params]))

(defn listen! [this component & params]
  (-listen! this component params))


(defprotocol IReadable
  (-read [this params]))

(defn read [this & params]
  (-read this params))


(defprotocol ITransact
  (-transact! [this params]))

(defn transact! [this & params]
  (-transact! this params))


(defprotocol ICommit
  (-commit [this]))

(defn commit [this]
  (-commit this))


(defprotocol INotificationSource
  (-notify-deps [this]))

(defn notify-deps [this]
  (-notify-deps this))


(defprotocol IDirty
  (-dirty? [this]))

(defn dirty? [this]
  (-dirty? this))


(defn is-store? [store]
  (and (instance? Store store)
       (satisfies? IListen store)
       (satisfies? IReadable store)
       (satisfies? ICommit store)
       (satisfies? INotificationSource store)
       (satisfies? IDirty store)
       (satisfies? ITransact store)
       (satisfies? IRemoveDep store)))
