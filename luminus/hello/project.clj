(defproject
  hello
  "luminus"
  :dependencies
  [[org.clojure/clojure "1.5.1"]
   [lib-noir "0.8.2"]
   [compojure "1.1.6"]
   [ring-server "0.3.1"]
   [selmer "0.5.7"]
   [com.taoensso/timbre "2.7.1"]
   [com.postspectacular/rotor "0.1.0"]
   [com.taoensso/tower "2.0.2"]
   [mysql/mysql-connector-java "5.1.28"]
   [korma "0.3.0-RC5"]
   [log4j
    "1.2.17"
    :exclusions
    [javax.mail/mail
     javax.jms/jms
     com.sun.jdmk/jmxtools
     com.sun.jmx/jmxri]]]
  :ring
  {:handler hello.handler/app,
   :init hello.handler/init,
   :destroy hello.handler/destroy}
  :profiles
  {:uberjar {:aot :all}
   :production
   {:ring
    {:open-browser? false, :stacktraces? false, :auto-reload? false}},
   :dev
   {:dependencies [[ring-mock "0.1.5"] [ring/ring-devel "1.2.2"]]}}
  :url
  "http://example.com/FIXME"
  :plugins
  [[lein-ring "0.8.10"]]
  :description
  "FIXME: write description"
  :min-lein-version "2.0.0")
