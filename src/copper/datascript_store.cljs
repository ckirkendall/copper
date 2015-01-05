(ns copper.datascript-store
  "This is a very old version of tonsky/datascript. I am using it because
   it is easy to test out ideas. Once the ideas prove out I will move
   to extending the current datascript."
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]
    [copper.core :as core]
    [copper.store :as store]
    [copper.basic-store :as bs]))

(declare transact-datom)

(def base ::base)
(def deps [base ::deps])
(def edit-queue [base ::edits])
(def last-edits [base ::last-edits])

(deftype Datom [e a v tx added _hash]
  Object
  (toString [_]
    (str "#datom[" (pr-str e a v tx added) "]"))

  ICloneable
  (-clone [_] (Datom. e a v tx added _hash))

  IHash
  (-hash [_] _hash)

  IEquiv
  (-equiv [_ o] (and (= e (.-e o)) (= a (.-a o)) (= v (.-v o))))

  ILookup
  (-lookup [_ k] (-lookup _ k nil))
  (-lookup [_ k not-found] (case k :e e :a a :v v :tx tx :added added not-found))

  ISequential
  ISeqable
  (-seq [_] [e a v tx added])

  ICounted
  (-count [coll] 5)

  IPrintWithWriter
  (-pr-writer [_ writer _] (-write writer (str "#datom[" (pr-str e a v tx added) "]"))))

(defn datom [e a v tx added]
  (Datom. e a v tx added (-> (hash e)
                             (hash-combine (hash a))
                             (hash-combine (hash v)))))

(defprotocol ISearch
  (-search [data pattern]))

(defn- match-tuple [tuple pattern]
  (every? true?
    (map #(or (nil? %2) (= %1 %2)) tuple pattern)))

(defn- search [data pattern]
  (cond
    (satisfies? ISearch data)
      (-search data pattern)
    (satisfies? ISeqable data)
      (filter #(match-tuple % pattern) data)))

(defn create-store [& [schema]]
  (let [ea (sorted-map)
        av (sorted-map)
        db (store/-store
            (atom {:ea ea
                   :av av
                   :schema schema}))]
    (reify
      ISearch
      (-search [this [e a v tx added :as pattern]]
        (cond->>
         (case [(when e :+) (when a :+) (when v :+)]
           [:+  nil nil]
           (->> (store/read this [:ea e]) vals (apply concat))
           [nil :+  nil]
           (->> (store/read this [:av a]) vals (apply concat))
           [:+  :+  nil]
           (store/read this [:ea e a])
           [nil :+  :+]
           (store/read this [:av a v])
           [:+  :+  :+]
           (->> (store/read this [:ea e a])
                (filter #(= v (.-v %)))))
         tx    (filter #(= tx (.-tx %)))
         added (filter #(= added (.-added %)))))

      store/IListen
      (-listen! [_ component path]
        (store/listen! db component [path]))
      
      store/IRemoveDep
      (-remove-dep! [_ component [path]]
        (store/remove-dep! db component path))
      
      store/IReadable
      (-read [this [path]]
        (store/listen! this core/*component* path)
        (get-in @db path))

      store/ITransact
      (-transact! [_ [datom]]
        (swap! db update-in edit-queue (fn [datoms]
                                         (core/add-to [] datoms datom))))

      store/ICommit
      (-commit [_]
        (swap! db (fn [state]
                    (let [edits (get-in state edit-queue)
                          new-state (reduce (fn [v datom]
                                              (transact-datom state datom))
                                            state
                                            edits)]
                      (-> new-state
                          (assoc-in last-edits edits)
                          (assoc-in edit-queue nil))))))

      store/INotificationSource
      (-notify-deps [_]
        (let [edits (get-in @db last-edits)
              dep-graph (get-in @db deps)
              ea-paths (map (fn [datom] [:ea (:e datom) (:a datom)]) edits)
              av-paths (map (fn [datom] [:ea (:a datom) (:v datom)]) edits)]
          (bs/notify-updates (concat ea-paths av-paths) dep-graph)))

      store/IDirty
      (-dirty? [_]
        (store/dirty? db)))))

(defn- update-in-sorted [map path f & args]
  (let [map (if (associative? map) map (sorted-map))
        [k & ks] path]
    (if ks
      (assoc map k (apply update-in-sorted (get map k) ks f args))
      (apply update-in map [k] f args))))

(defn- transact-datom [db datom]
  (if (.-added datom)
    (-> db
      (update-in-sorted [:ea (.-e datom) (.-a datom)] (fnil conj #{}) datom)
      (update-in-sorted [:av (.-a datom) (.-v datom)] (fnil conj #{}) datom))
    (-> db
      (update-in-sorted [:ea (.-e datom) (.-a datom)] disj datom)
      (update-in-sorted [:av (.-a datom) (.-v datom)] disj datom))))

(defn- explode-entity [db entity]
  (if (map? entity)
    (let [eid (:db/id entity)]
      (for [[a vs] (dissoc entity :db/id)
            v      (if (and (sequential? vs)
                            (= :many (get-in db [:schema a :cardinality])))
                     vs
                     [vs])]
        [:db/add eid a v]))
    [entity]))

(defn- op->datoms [db [op e a v] tx]
  (case op
    :db/add
      (if (= :many (store/read db [:schema a :cardinality]))
        (when (empty? (-search db [e a v]))
          [(datom e a v tx true)])
        (if-let [old-datom (first (-search db [e a]))]
          (when (not= (.-v old-datom) v)
            [(datom e a (.-v old-datom) tx false)
             (datom e a v tx true)])
          [(datom e a v tx true)]))
    :db/retract
      (when-let [old-datom (first (-search db [e a v]))]
        [(datom e a v tx false)])))

(defn transact! [db entities]
  (let [tx     0
        datoms (->> entities
                    (mapcat #(explode-entity db %))
                    (mapcat #(op->datoms db % tx)))]
    (doseq [datom datoms]
      (store/transact! db datom nil))))

(defn next-eid [db & [offset]]
  (let [max-eid (or (-> (:ea db) keys last) 0)]
    (+ max-eid (or offset 1))))


;; QUERIES

(defn- parse-where [where]
  (let [source (first where)]
    (if (and (symbol? source)
             (= \$ (-> source name first)))
      [(first where) (next where)]
      ['$ where])))

(defn- bind-symbol [sym scope]
  (cond
    (= '_ sym)    nil
    (symbol? sym) (get scope sym nil)
    :else         sym))

(defn- bind-symbols [form scope]
  (map #(bind-symbol % scope) form))

(defn- search-datoms [source where scope]
  (search (bind-symbol source scope)
          (bind-symbols where scope)))

(defn- populate-scope [scope where datom]
  (->>
    (map #(when (and (symbol? %1)
                     (not (contains? scope %1)))
            [%1 %2])
      where
      datom)
    (remove nil?)
    (into scope)))

(defn- -differ? [& xs]
  (let [l (count xs)]
    (not= (take (/ l 2) xs) (drop (/ l 2) xs))))

(def ^:private built-ins { '= =, '== ==, 'not= not=, '!= not=, '< <, '> >, '<= <=, '>= >=, '+ +, '- -, '* *, '/ /, 'quot quot, 'rem rem, 'mod mod, 'inc inc, 'dec dec, 'max max, 'min min,
                           'zero? zero?, 'pos? pos?, 'neg? neg?, 'even? even?, 'odd? odd?, 'true? true?, 'false? false?, 'nil? nil?,
                           '-differ? -differ?})

(defn- call [[f & args] scope]
  (let [bound-args (bind-symbols args scope)
        f          (or (built-ins f) (scope f))]
    (apply f bound-args)))

(defn- looks-like? [pattern form]
  (cond
    (= '_ pattern)
      true
    (= '[*] pattern)
      (sequential? form)
    (sequential? pattern)
      (and (sequential? form)
           (= (count form) (count pattern))
           (every? (fn [[pattern-el form-el]] (looks-like? pattern-el form-el))
                   (map vector pattern form)))
    (symbol? pattern)
      (= form pattern)
    :else ;; (predicate? pattern)
      (pattern form)))

(defn collect [f coll]
  (reduce #(set/union %1 (f %2)) #{} coll))

(defn bind-rule-branch [branch call-args context]
  (let [[[rule & local-args] & body] branch
        replacements (zipmap local-args call-args)
        ;; replacing free vars to unique symbols
        seqid        (:__depth context 0)
        bound-body   (walk/postwalk #(if (and (symbol? %)
                                              (= \? (-> % name first)))
                                       (or (replacements %)
                                           (symbol (str (name %) "__auto__" seqid)))
                                       %)
                                     body)]
    ;; recursion breaker
    ;; adding condition that call args cannot take same values as they took in any previous call to this rule
    (concat
      (for [prev-call-args (get context rule)]
        [(concat ['-differ?] call-args prev-call-args)])
      bound-body)))

(defn -q [in+sources wheres scope]
  (cond
    (not-empty in+sources) ;; parsing ins
      (let [[in source] (first in+sources)]
        (condp looks-like? in
          '[_ ...] ;; collection binding [?x ...]
            (collect #(-q (concat [[(first in) %]] (next in+sources)) wheres scope) source)

          '[[*]]   ;; relation binding [[?a ?b]]
            (collect #(-q (concat [[(first in) %]] (next in+sources)) wheres scope) source)

          '[*]     ;; tuple binding [?a ?b]
            (recur (concat
                     (zipmap in source)
                     (next in+sources))
                   wheres
                   scope)
          '%       ;; rules
            (recur (next in+sources)
                   wheres
                   (assoc scope :__rules (group-by ffirst source)))

          '_       ;; regular binding ?x
            (recur (next in+sources)
                   wheres
                   (assoc scope in source))))

    (not-empty wheres) ;; parsing wheres
      (let [where (first wheres)]
        
        ;; rule (rule ?a ?b ?c)
        (if-let [rule-branches (get (:__rules scope) (first where))]
          (let [[rule & call-args] where
                next-scope (-> scope
                             (update-in [:__rules_ctx rule] conj call-args)
                             (update-in [:__rules_ctx :__depth] inc))
                next-wheres (next wheres)]
            (collect
              #(-q nil
                   (concat (bind-rule-branch % call-args (:__rules_ctx scope)) next-wheres)
                   next-scope)
              rule-branches))
          
          (condp looks-like? where
            '[[*]] ;; predicate [(pred ?a ?b ?c)]
              (when (call (first where) scope)
                (recur nil (next wheres) scope))

            '[[*] _] ;; function [(fn ?a ?b) ?res]
              (let [res (call (first where) scope)]
                (recur [[(second where) res]] (next wheres) scope))

            '[*] ;; pattern
              (let [[source where] (parse-where where)
                    found          (search-datoms source where scope)]
                (collect #(-q nil (next wheres) (populate-scope scope where %)) found))
            )))
   
   :else ;; reached bottom
      #{(mapv scope (:__find scope))}
    ))


;; AGGREGATES

(def ^:private built-in-aggregates {
  'distinct (comp vec distinct)
  'min    (fn
            ([coll] (reduce min coll))
            ([n coll]
              (vec
                (reduce (fn [acc x]
                          (cond
                            (< (count acc) n) (sort (conj acc x))
                            (< x (last acc))  (sort (conj (butlast acc) x))
                            :else             acc))
                        [] coll))))
  'max    (fn
            ([coll] (reduce max coll))
            ([n coll]
              (vec
                (reduce (fn [acc x]
                          (cond
                            (< (count acc) n) (sort (conj acc x))
                            (> x (first acc)) (sort (conj (next acc) x))
                            :else             acc))
                        [] coll))))
  'sum    #(reduce + 0 %)
  'rand   (fn
            ([coll] (rand-nth coll))
            ([n coll] (vec (repeatedly n #(rand-nth coll)))))
  'sample (fn [n coll]
            (vec (take n (shuffle coll))))
  'count  count})

(defn- aggr-group-key [find result]
  (mapv (fn [val sym]
          (if (sequential? sym) nil val))
        result
        find))

(defn- -aggregate [find scope results]
  (mapv (fn [sym val i]
          (if (sequential? sym)
            (let [[f & args] sym
                  vals (map #(get % i) results)
                  args (concat
                        (bind-symbols (butlast args) scope)
                        [vals])
                  f    (or (built-in-aggregates f) (scope f))]
              (apply f args))
            val))
        find
        (first results)
        (range)))

(defn- aggregate [query scope results]
  (let [find (concat (:find query) (:with query))]
    (->> results
         (group-by #(aggr-group-key find %))
         (mapv (fn [[_ results]] (-aggregate (:find query) scope results))))))


;; SUMMING UP

(defn q [query & sources]
  (let [ins->sources (zipmap (:in query '[$]) sources)
        find         (concat
                       (map #(if (sequential? %) (last %) %) (:find query))
                       (:with query))
        results      (-q ins->sources (:where query) {:__find find})]
    (cond->> results
      (:with query)
        (mapv #(subvec % 0 (count (:find query))))
      (not-empty (filter sequential? (:find query)))
        (aggregate query ins->sources))))
