(ns copper.test-util
  (:require [clojure.string :as str]))
 

(defn root-element [id]
  (let [div (.createElement js/document "div")]
    (.setAttribute div "id" id)
    (.appendChild (.-body js/document) div)))


(defn by-id [id]
  (.getElementById js/document id))


(defn simulate-click-event [target]
  (js/React.addons.TestUtils.Simulate.click target))


(defn simulate-change-event [target]
  (js/React.addons.TestUtils.Simulate.change target))


(defn attr-pattern
  "Returns a regular expression that matches the HTML attribute `attr`
  and it's value."
  [attr]
  (re-pattern (str "\\s+" (name attr) "\\s*=\\s*['\"][^\"']+['\"]")))


(defn strip-attr
  "Strip the HTML attribute `attr` and it's value from the string `s`."
  [s attr]
  (if s (str/replace s (attr-pattern attr) "")))


(defn clean-html [html-str]
  (-> html-str
      (strip-attr :data-reactid)
      (strip-attr :data-react-checksum)))
