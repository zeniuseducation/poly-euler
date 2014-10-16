(defproject
  zenrepo
  "0.1.0-SNAPSHOT"
  :repl-options
  {:init-ns zenrepo.repl}
  :dependencies
  [[ring-server "0.3.1"]
   [noir-exception "0.2.2"]
   [environ "0.5.0"]
   [com.taoensso/timbre "3.2.1"]
   [markdown-clj "0.9.44"]
   [org.clojure/clojure "1.6.0"]
   [com.taoensso/tower "2.0.2"]
   [http-kit "2.1.18"]
   [selmer "0.6.8"]
   [lib-noir "0.8.4"]
   [me.raynes/fs "1.4.4"]]
  :ring
  {:handler zenrepo.handler/app,
   :init zenrepo.handler/init,
   :destroy zenrepo.handler/destroy}
  :profiles
  {:uberjar {:aot :all},
   :production
   {:ring
    {:open-browser? false, :stacktraces? false, :auto-reload? false}},
   :dev
   {:dependencies [[ring-mock "0.1.5"] [ring/ring-devel "1.3.0"]],
    :env {:dev true}}}
  :url
  "http://example.com/FIXME"
  :main
  zenrepo.core
  :plugins
  [[lein-ring "0.8.10"] [lein-environ "0.5.0"]]
  :description
  "FIXME: write description"
  :min-lein-version "2.0.0")