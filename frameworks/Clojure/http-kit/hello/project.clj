(defproject hello "compojure"
  :description "JSON/Database tests"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [ring/ring-json "0.2.0"]
                 [org.clojure/tools.cli "0.2.1"]
                 [http-kit/dbcp "0.1.0"]
                 [http-kit "2.1.18"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 ; [ch.qos.logback/logback-classic "1.1.2" :exclusions [org.slf4j/slf4j-api]]
                 ; [org.slf4j/jul-to-slf4j "1.7.7"]
                 ; [org.slf4j/jcl-over-slf4j "1.7.7"]
                 ; [org.slf4j/log4j-over-slf4j "1.7.7"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/java.jdbc "0.3.6"]
                 [korma "0.4.0"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [hiccup "1.0.4"]]
  :main hello.handler
  :aot [hello.handler]
  :uberjar-name "http-kit-standalone.jar"
  :profiles {:dev {:dependencies [[ring-mock "0.1.5"]]}})
