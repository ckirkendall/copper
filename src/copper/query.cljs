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
                    (core/transact! res (vec rpaths) func)
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
