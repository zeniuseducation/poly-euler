(defproject poly-euler "0.1.0"
  :description "Polyglottic euler playground in Clojure, CLisp, & Haskell"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [expectations "2.0.9"]]
  :main ^:skip-aot poly-euler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
