(defproject hello "compojure"
  :description "JSON/Database tests"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.1.5"]
                 [ring/ring-json "0.1.2"]
                 [korma "0.3.0-RC2"]
                 [mysql/mysql-connector-java "5.1.6"]
                 ]
  :plugins [[lein-ring "0.8.2"]]
  :ring {:handler hello.handler/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]]}})
