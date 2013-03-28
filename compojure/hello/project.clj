(defproject hello "compojure"
  :description "JSON/Database tests"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [ring/ring-json "0.2.0"]
                 [korma "0.3.0-RC4"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [mysql/mysql-connector-java "5.1.6"]
                 ]
  :plugins [[lein-ring "0.8.2"]]
  :ring {:handler hello.handler/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]]}})
