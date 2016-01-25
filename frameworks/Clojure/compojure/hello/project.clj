(defproject hello "compojure"
  :description "FrameworkBenchmarks test implementations"
  :url "http://localhost:3000/"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [korma "0.4.2"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [mysql/mysql-connector-java "5.1.38"]
                 [org.clojure/java.jdbc "0.3.7"]
                 [hikari-cp "1.5.0"]
                 [hiccup "1.0.5"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler hello.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
