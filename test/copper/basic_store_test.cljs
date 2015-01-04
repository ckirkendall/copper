(ns copper.basic-store-test
  (:require [cemerick.cljs.test :as t]
            [copper.core :as c]
            [copper.test-util :as tu]
            [copper.basic-store :refer [query sub-cursor]])
  (:require-macros [cemerick.cljs.test :refer [deftest testing is done]]))

 
(deftest read-test
  (testing "simple read on store"
    (let [state (c/create-store (atom {:a {:b :c}}))]
      (is (= :c (c/read state [:a :b])))
      (is (= nil (c/read state [:x])))))
  (testing "read on clojure map"
    (let [state {:a {:b :c}}]
      (is (= :c (c/read state [:a :b])))
      (is (= nil (c/read state [:x])))))
  (testing "read on clojure map"
    (let [state [:a [:b]]]
      (is (= :b (c/read state [1 0]))))))


(deftest update-commit-test
  (testing "simple transact"
    (let [state (c/create-store (atom {:a {:b :fail}}))]
      (c/update! state [:a :b] :pass)
      (is (not= :pass (c/read state [:a :b])))
      (c/commit state)
      (is (= :pass (c/read state [:a :b]))))))      


(deftest notify-test
  (testing "basic positive notify"
    (let [state (c/create-store (atom {:a {:b :c}}))
          component (specify! #js{:state :failed}
                      c/INotify
                      (-notify [this path]
                               (aset this "state" :passed))
                      IPrintWithWriter
                      (-pr-writer [this writer _]
                         (-write writer (pr-str "COMP" {:state (.-state this)}))))]
      (is (= :c (binding [c/*component* component]
                  (c/read state [:a :b]))))
      (c/update! state [:a :b] :pass)
      (is (= :failed (.-state component)))
      (c/commit state)
      (c/notify-deps state)
      (is (= :passed (.-state component))))))


(deftest query-test
  (testing "simple query"
    (let [state (c/create-store (atom {:a [1 2 3]}))
          pcur (query '{:build {:val ($ ?a)}
                        :from [?a]}
                      (sub-cursor state :a))]
      (is (= 1 (c/read pcur [0 :val])))
      (is (= 2 (c/read pcur [1 :val])))
      (is (= 3 (c/read pcur [2 :val])))))


  (testing "query with seq breakdown & transact"
    (let [state (c/create-store (atom {:a [1 2 3]}))
          pcur (query '{:build {:val ($ ?a)}
                        :from [?a]}
                      (sub-cursor state :a))]
      (doseq [p pcur]
        (c/transact! p [:val] inc))
      (c/commit state)
      (is (= [2 3 4]
             (c/read state [:a])
             (for [p pcur]
               (c/read p [:val]))))))

  (testing "simple query with where clause"
    (let [state (c/create-store (atom {:a [1 2 3]}))
          pcur (query '{:build {:val ($ ?a)}
                        :from [?a]
                        :where [(> (read ?a) 1)]}
                      (sub-cursor state :a))]
      (is (= 2 (count pcur)))
      (is (= 2 (c/read pcur [0 :val])))
      (is (= 3 (c/read pcur [1 :val])))
      (is (= [2 3]
             (for [p pcur]
               (c/read p [:val])))))))


