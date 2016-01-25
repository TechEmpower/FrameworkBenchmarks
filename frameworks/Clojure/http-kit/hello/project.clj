(defproject hello "http-kit"
  :description "FrameworkBenchmarks test implementations"
  :url "http://localhost:8080/"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [org.clojure/tools.cli "0.2.1"]
                 [http-kit "2.1.19"]
                 [korma "0.4.2"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 ; [ch.qos.logback/logback-classic "1.1.2" :exclusions [org.slf4j/slf4j-api]]
                 ; [org.slf4j/jul-to-slf4j "1.7.7"]
                 ; [org.slf4j/jcl-over-slf4j "1.7.7"]
                 ; [org.slf4j/log4j-over-slf4j "1.7.7"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [mysql/mysql-connector-java "5.1.38"]
                 [org.clojure/java.jdbc "0.3.7"]
                 [hikari-cp "1.5.0"]
                 [hiccup "1.0.5"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler hello.handler/app}
  :main hello.handler
  :aot [hello.handler]
  :uberjar-name "http-kit-standalone.jar"
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
