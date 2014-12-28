(ns copper.query-test
  (:require [cemerick.cljs.test :as t]
            [copper.core :as c]
            [copper.test-util :as tu]
            [copper.query :as q :refer [query]])
  (:require-macros [cemerick.cljs.test :refer [deftest testing is done]]))


(deftest query-test
  (testing "simple query on clojure map"
    (let [state (c/create-store (atom {:a [1 2 3]}))
          pcur (query '{:build {:val ($ ?a)}
                        :from [?a]}
                      (c/sub-cursor state :a))]
      (is (= 1 (c/read pcur [0 :val])))
      (is (= 2 (c/read pcur [1 :val])))
      (is (= 3 (c/read pcur [2 :val]))))))
