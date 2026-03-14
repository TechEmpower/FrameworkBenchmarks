(defproject hello "compojure"
  :description "FrameworkBenchmarks test implementations"
  :url "http://localhost:3000/"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.12.4"]
                 [compojure "1.7.2"]
                 [ring/ring-json "0.5.1"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [com.mysql/mysql-connector-j "9.6.0"]
                 [org.clojars.jj/majavat "1.19.0"]
                 [com.github.seancorfield/next.jdbc "1.3.1093"]
                 [hikari-cp "4.0.0"]
                 [hiccup "1.0.5"]]
  :repositories {"Sonatype releases" "https://oss.sonatype.org/content/repositories/releases/"}
  :plugins [[lein-ring "0.12.5"]
            [org.clojure/core.unify "0.5.7"]
            [nightlight/lein-nightlight "RELEASE"]]
  :ring {:handler hello.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
