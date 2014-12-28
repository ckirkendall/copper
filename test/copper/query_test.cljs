(ns copper.query-test
  (:require [cemerick.cljs.test :as t]
            [copper.core :as c]
            [copper.test-util :as tu]
            [copper.query :as q :refer [query]])
  (:require-macros [cemerick.cljs.test :refer [deftest testing is done]]))


(deftest query-test
  (testing "simple query"
    (let [state (c/create-store (atom {:a [1 2 3]}))
          pcur (query '{:build {:val ($ ?a)}
                        :from [?a]}
                      (c/sub-cursor state :a))]
      (is (= 1 (c/read pcur [0 :val])))
      (is (= 2 (c/read pcur [1 :val])))
      (is (= 3 (c/read pcur [2 :val])))))
  
  (testing "query with seq breakdown"
    (let [state (c/create-store (atom {:a [1 2 3]}))
          pcur (query '{:build {:val ($ ?a)}
                        :from [?a]}
                      (c/sub-cursor state :a))]
      (is (= 3 (count pcur)))
      (is (= [1 2 3]
             (for [p pcur]
               (c/read p [:val]))))))

  (testing "query with seq breakdown & transact"
    (let [state (c/create-store (atom {:a [1 2 3]}))
          pcur (query '{:build {:val ($ ?a)}
                        :from [?a]}
                      (c/sub-cursor state :a))]
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
                      (c/sub-cursor state :a))]
      (is (= 2 (count pcur)))
      (is (= 2 (c/read pcur [0 :val])))
      (is (= 3 (c/read pcur [1 :val])))
      (is (= [2 3]
             (for [p pcur]
               (c/read p [:val])))))))


