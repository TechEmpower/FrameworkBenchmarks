(defproject hello "http-kit"
  :description "FrameworkBenchmarks test implementations"
  :url "http://localhost:8080/"
  :dependencies [[org.clojure/clojure "1.12.4"]
                 [compojure "1.7.2"]
                 [ring/ring-json "0.5.1"]
                 [org.clojure/tools.cli "0.2.1"]
                 [http-kit "2.8.1"]
                 [javax.xml.bind/jaxb-api "2.2.12"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [com.mysql/mysql-connector-j "9.6.0"]
                 [com.github.seancorfield/next.jdbc "1.3.1093"]
                 [hikari-cp "4.0.0"]
                 [hiccup "1.0.5"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler hello.handler/app}
  :main hello.handler
  :aot [hello.handler]
  :uberjar-name "http-kit-standalone.jar"
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
