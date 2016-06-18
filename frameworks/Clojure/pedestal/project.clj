(defproject pedestal "0.1"
  :description "A Clojure-Pedestal server for testing in the Framework Benchmarks"
  :url "https://github.com/TechEmpower/FrameworkBenchmarks"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [io.pedestal/pedestal.service "0.4.1"]
                 [io.pedestal/pedestal.jetty "0.4.1"]
                 [ch.qos.logback/logback-classic "1.1.2" :exclusions [org.slf4j/slf4j-api]]
                 [org.slf4j/jul-to-slf4j "1.7.7"]
                 [org.slf4j/jcl-over-slf4j "1.7.7"]
                 [org.slf4j/log4j-over-slf4j "1.7.7"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/java.jdbc "0.4.2"]
                 [korma "0.4.0"]
                 [mysql/mysql-connector-java "5.1.38"]
                 [hiccup "1.0.5"]]
  :min-lein-version "2.0.0"
  :resource-paths ["config", "resources"]
  :profiles {:dev {:aliases {"run-dev" ["trampoline" "run" "-m" "pedestal.server/run-dev"]}
                   :dependencies [[io.pedestal/pedestal.service-tools "0.3.1"]]}}
  :auto-clean false
  :main pedestal.server
  :aot [pedestal.server]
  :uberjar-name "pedestal-standalone.jar")
