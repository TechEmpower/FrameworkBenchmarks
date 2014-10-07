(defproject hello "compojure"
  :description "JSON/Database tests"
  :url "http://localhost:3000/"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.6"]
                 [ring/ring-json "0.2.0"]
                 [korma "0.3.0-RC6"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [mysql/mysql-connector-java "5.1.6"]
                 [org.clojure/java.jdbc "0.3.0-alpha1"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [hiccup "1.0.4"]
                 ]
  :plugins [[lein-ring "0.8.10"]]
  :ring {:handler hello.handler/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.5"]]}})
