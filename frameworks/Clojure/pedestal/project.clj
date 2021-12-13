(defproject pedestal "0.1"
  :description "A Clojure-Pedestal server for testing in the Framework Benchmarks"
  :url "https://github.com/TechEmpower/FrameworkBenchmarks"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [io.pedestal/pedestal.service "0.5.2"]
                 [io.pedestal/pedestal.jetty "0.5.2"]
                 [org.clojure/java.jdbc "0.4.2"]
                 [korma "0.4.2"]
                 [mysql/mysql-connector-java "8.0.18"]
                 [com.zaxxer/HikariCP "2.5.1" :exclusions [[org.slf4j/slf4j-api]]]
                 [hiccup "1.0.5"]]
  :min-lein-version "2.0.0"
  :resource-paths ["config"]
  :profiles {:uberjar {:global-vars ^:replace {*warn-on-reflection* true
                                               *unchecked-math* :warn-on-boxed
                                               ;*compiler-options* {:disable-locals-clearing true}
                                               *assert* false}
                       :jvm-opts ["-D\"clojure.compiler.direct-linking=true\""
                                  "-D\"io.pedestal.log.overrideLogger=nil\""
                                  "-D\"io.pedestal.log.defaultMetricsRecorder=nil\""]
                       :aot [pedestal.pdg]
                       :main pedestal.pdg}
             :srepl {:jvm-opts ^:replace ["-d64" "-server"
                                          "-XX:+UseG1GC"
                                          "-D\"clojure.compiler.direct-linking=true\""
                                          "-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]}
             :dev {:aliases {"crepl" ["trampoline" "run" "-m" "clojure.main/main"]
                             "srepl" ["with-profile" "srepl" "trampoline" "run" "-m" "clojure.main/main"]
                             "run-dev" ["trampoline" "run" "-m" "pedestal.pdg/run-dev"]}
                   :dependencies [;[io.pedestal/pedestal.service-tools "0.5.2"]
                                  [ch.qos.logback/logback-classic "1.1.3" :exclusions [org.slf4j/slf4j-api]]
                                  [org.slf4j/jul-to-slf4j "1.7.22"]
                                  [org.slf4j/jcl-over-slf4j "1.7.22"]
                                  [org.slf4j/log4j-over-slf4j "1.7.22"]
                                  [criterium "0.4.4"]]}}
  :auto-clean false
  :jvm-opts ^:replace [;; Turn on Clojure's Direct Linking
                       "-D\"clojure.compiler.direct-linking=true\""
                       ;; Turn off Pedestal's Metrics
                       "-D\"io.pedestal.defaultMetricsRecorder=nil\""
                       ;"-d64" "-server"
                       "-Xms1g"                             ;"-Xmx1g"
                       ;"-XX:+UnlockCommercialFeatures"      ;"-XX:+FlightRecorder"
                       ;"-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8030"
                       "-XX:+UseG1GC"
                       ;"-XX:+UseConcMarkSweepGC" "-XX:+UseParNewGC" "-XX:+CMSParallelRemarkEnabled"
                       ;"-XX:+ExplicitGCInvokesConcurrent"
                       "-XX:+AggressiveOpts"
                       ;-XX:+UseLargePages
                       "-XX:+UseCompressedOops"]
  :global-vars {*warn-on-reflection* true
                *unchecked-math* :warn-on-boxed
                ;*compiler-options* {:disable-locals-clearing true}
                *assert* true}
  :pedantic? :abort
  :main ^{:skip-aot true} pedestal.pdg
  :uberjar-name "pedestal-standalone.jar")

