(ns copper.core-test
  (:require [cemerick.cljs.test :as t]
            [copper.core :as c]
            [copper.store :as s]
            [copper.basic-store :as bs]
            [copper.test-util :as tu]
            [copper.dom :as dom])
  (:require-macros [cemerick.cljs.test :refer [deftest testing is done]]))

;;pollyfil requestAnimationFrame
(set! (.-requestAnimationFrame js/window) #(js/setTimeout % 30))

(defn clean-html [html-str]
  (-> html-str
      (tu/strip-attr :data-reactid)
      (tu/strip-attr :data-react-checksum)))


(deftest ^:async basic-render-test
  (testing "simple render test with state"
    (let [root-element (tu/root-element "r1")
          app-state (c/create-store (atom {:a {:b :fail}}))
          root-c (c/component
                  (fn [_ _]
                    (dom/button #js{:id "button1"
                                    :onClick #(c/transact! app-state [:a :b] (fn [_] :pass))})))
          root (c/root #{app-state} root-c root-element {})]
      (tu/simulate-click-event (tu/by-id "button1"))
      (js/setTimeout
       (fn []
         (is (= :pass (c/read app-state [:a :b])))
         (c/unmount root) )
       100)))
  
  (testing "simple render test with state & notify"
    (let [root-element (tu/root-element "r2")
          app-state (c/create-store (atom {:a {:b :fail}}))
          root-c (c/component
                  (fn [_ _]
                    (let [r (c/read app-state [:a :b])]
                      (dom/span nil
                        (dom/span #js{:id "s2"}
                          (name r))
                          (dom/button #js{:id "button2"
                                          :onClick #(c/transact! app-state [:a :b] (fn [_] :pass))})))))
          root (c/root root-c root-element {})]
      (c/register-store! root app-state)
      (is (= "fail" (.-innerHTML (tu/by-id "s2"))))
      (tu/simulate-click-event (tu/by-id "button2"))
      (js/setTimeout
       (fn []
         (is (= :pass (c/read app-state [:a :b])))
         (is (= "pass" (.-innerHTML (tu/by-id "s2"))))
         (c/unmount root) 
         (done))
       200)))) 


