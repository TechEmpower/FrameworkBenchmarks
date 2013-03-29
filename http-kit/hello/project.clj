(defproject hello "compojure"
  :description "JSON/Database tests"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [ring/ring-json "0.2.0"]
                 [org.clojure/tools.cli "0.2.1"]
                 [http-kit/dbcp "0.1.0"]
                 [http-kit "2.0.0-RC4"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [mysql/mysql-connector-java "5.1.6"]]
  :main hello.handler
  :profiles {:dev {:dependencies [[ring-mock "0.1.3"]]}})
