(defproject
  hello
  "luminus"
  :dependencies
  [[org.clojure/clojure "1.5.1"]
   [lib-noir "0.6.6"]
   [compojure "1.1.5"]
   [ring-server "0.2.8"]
   [selmer "0.3.2"]
   [com.taoensso/timbre "1.6.0"]
   [com.postspectacular/rotor "0.1.0"]
   [com.taoensso/tower "1.5.1"]
   [markdown-clj "0.9.29"]
   [org.clojure/java.jdbc "0.2.3"]
   [mysql/mysql-connector-java "5.1.6"]
   [korma "0.3.0-RC5"]
   [log4j
    "1.2.15"
    :exclusions
    [javax.mail/mail
     javax.jms/jms
     com.sun.jdmk/jmxtools
     com.sun.jmx/jmxri]]]
  :ring
  {:handler hello.handler/war-handler,
   :init hello.handler/init,
   :destroy hello.handler/destroy}
  :profiles
  {:production
   {:ring
    {:open-browser? false, :stacktraces? false, :auto-reload? false}},
   :dev
   {:dependencies [[ring-mock "0.1.3"] [ring/ring-devel "1.1.8"]]}}
  :url
  "http://example.com/FIXME"
  :plugins
  [[lein-ring "0.8.5"]]
  :description
  "FIXME: write description"
  :min-lein-version "2.0.0")
