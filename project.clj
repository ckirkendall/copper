(defproject copper "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"] 
                 [org.clojure/clojurescript "0.0-2505" :scope "provided"]
                 [com.facebook/react "0.12.2.1"]]
  :plugins [[com.cemerick/clojurescript.test "0.3.2-SNAPSHOT"]
            [lein-cljsbuild "1.0.4-SNAPSHOT"]
            [lein-ancient "0.5.4"]]
  :cljsbuild {:builds []}
  :aliases {"auto-build" ["do" "clean"
                          ["garden" "once"]
                          ["cljsbuild" "auto" "dev"]]
            "auto-test" ["do" "clean"
                         ["cljsbuild" "auto" "test"]]}

  :profiles
  {:dev {:source-paths ["src" "dev"]
         :dependencies [[com.cemerick/piggieback "0.1.3"]
                        [weasel "0.4.0-SNAPSHOT"]]
         :plugins [[com.cemerick/clojurescript.test "0.3.2-SNAPSHOT"]
                   [lein-cljsbuild "1.0.4-SNAPSHOT"]
                   [lein-ancient "0.5.4"]]
         :repl-options  {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
         :cljsbuild {:builds [{:id "dev"
                               :source-paths ["src" "dev"]
                               :compiler {:output-to "resources/public/js/copper.js"
                                          :output-dir "resources/public/out"
                                          :optimizations :none
                                          :pretty-print true
                                          :source-map true}}
                              {:id "test" 
                               :notify-command ["phantomjs" :cljs.test/runner "target/test/copper.js"]
                               :source-paths ["test" "src"]
                               :compiler {:output-to "target/test/copper.js"
                                          :optimizations :whitespace
                                          :pretty-print true
                                          :preamble ["phantomjs-shims.js"
                                                     "react/react_with_addons.js"]}}]
                     :test-commands {"phantom" ["phantomjs" :runner "target/test/copper.js"]}}}})
