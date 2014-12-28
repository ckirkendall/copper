(ns copper.query
  (:require [copper.core :as core :refer [read transact! sub-cursor]]
            [clojure.walk :as walk]))


(comment
  (query '{:build {:value ($ ?a :id) :display ($ ?a :id)}
           :from [?a ?b *v]
           :where [(= (read ?a [:id]) (read ?b [:id]))
                   (= (read ?b [:tm]) *v)]}
         ($ store :a)
         ($ store :b)
         3))


(defrecord ProxyCursor [source]
  core/IReadable
  (-read [this path]
    (let [f (fn temp [source [path-part  & rpaths]]
             (let [res (get source path-part)]
               (if (satisfies? core/ITransact res)
                 (read res (vec rpaths))
                 (temp res rpaths))))]
      (f source path)))

  core/ITransact
  (-transact! [this path func]
    (let [f (fn temp [source [path-part  & rpaths]]
             (let [res (get source path-part)]
               (if (satisfies? core/ITransact res)
                 (core/transact! res (vec rpaths) func)
                 (temp res rpaths))))]
      (f source path))))

 
(def built-ins
  {'= =, '== ==, 'not= not=, '!= not=, '< <, '> >, '<= <=, '>= >=, '+ +, '- -,
   '* *, '/ /, 'quot quot, 'rem rem, 'mod mod, 'inc inc, 'dec dec, 'max max, 'min min,
   'zero? zero?, 'pos? pos?, 'neg? neg?, 'even? even?, 'odd? odd?, 'true? true?,
   'false? false?, 'nil? nil?, 'str str, '$ sub-cursor, 'read read
   'list list, 'vector vector})


(defn rset-generator [cursor]
  (let [cnt (count (read cursor))]
    (vec (for [idx (range cnt)]
           (sub-cursor cursor cnt)))))


(defn value-generator [value] [value])


(defn eval-form [form ctx]
  (walk/postwalk
   (fn [node]
     (cond
      (symbol? node)
      (get ctx node (get built-ins node))
      
      (list? node)
      (apply (first node) (rest node))

      :else node))
   form))


(defn build-fn [build]
  (fn [ctx]
    (eval-form build ctx)))


(defn where-fn [where]
  (fn [ctx]
    (every? identity
            (eval-form where ctx))))


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
                           (proc rfroms (assoc sym v ctx)))
                         vals)
                 (when (where-fn ctx) ctx)))]
    (ProxyCursor. (mapv build-fn (filt froms {})))))


(defn query [{:keys [build from where]} & args]
  (let [build-fn (build-fn build)
        where-fn (where-fn build)
        froms (from-args from args)]
    (generate-results froms where-fn build-fn)))
