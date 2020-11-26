(defproject hello "compojure"
  :description "FrameworkBenchmarks test implementations"
  :url "http://localhost:3000/"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [korma "0.5.0-RC1"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [mysql/mysql-connector-java "8.0.18"]
                 [com.mchange/c3p0 "0.9.5.4"]
                 [org.clojure/java.jdbc "0.7.9"]
                 [hikari-cp "1.8.3"]
                 [hiccup "1.0.5"]]
  :repositories {"Sonatype releases" "https://oss.sonatype.org/content/repositories/releases/"}
  :plugins [[lein-ring "0.12.5"]
            [org.clojure/core.unify "0.5.7"]
            [nightlight/lein-nightlight "RELEASE"]]
  :ring {:handler hello.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
