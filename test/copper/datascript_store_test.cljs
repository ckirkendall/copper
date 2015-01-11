(ns copper.datascript-store-test
  (:require [cemerick.cljs.test :as t]
            [copper.core :as c]
            [copper.store :as s]
            [copper.test-util :as tu]
            [copper.datascript-store :as d])
  (:require-macros [cemerick.cljs.test :refer [deftest testing is done]]))


(deftest basic-query-test
  (let [db (d/create-store  {:aka { :db/cardinality :db.cardinality/many }})]
    (s/transact! db [[:db/add 1 :name "Ivan"]
                     [:db/add 1 :name "Petr"]
                     [:db/add 1 :aka  "Devil"]
                     [:db/add 1 :aka  "Tupen"]])
    (s/commit db)
    (is (= #{[1]}  (d/q '[:find ?v
                         :where [?v :name "Petr"]] @db)))
    (is (= (d/q '[ :find ?v
                  :where [1 :name ?v]] @db)
           #{["Petr"]}))
    (is (= (d/q '[:find ?v
                  :where [1 :aka ?v]] @db)
           #{["Devil"] ["Tupen"]}))))



(deftest notify-test
  (let [db  (d/create-store {:aka { :db/cardinality :db.cardinality/many }})]
    (s/transact! db [[:db/add 1 :name "Ivan"]
                     [:db/add 1 :aka  "Devil"]
                     [:db/add 1 :aka  "Tupen"]])
    (s/commit db)
    (testing "basic positive notify"
      (let [component (specify! #js{:state :failed}
                                c/INotify
                                (-notify [this path]
                                         (aset this "state" :passed))
                                IPrintWithWriter
                                (-pr-writer [this writer _]
                                            (-write writer (pr-str "COMP" {:state (.-state this)}))))]
        (is (= #{["Ivan"]}
               (binding [c/*component* component]
                 (d/q '[:find ?v
                        :where [1 :name ?v]] @db))))
        (s/transact! db [[:db/add 1 :name "Petr"]])
        (is (= :failed (.-state component)))
        (s/commit db)
        (s/notify-deps db)
        (is (= :passed (.-state component)))))))
