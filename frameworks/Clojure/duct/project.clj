(defproject hello "duct"
  :description "FrameworkBenchmarks test implementations"
  :url "http://localhost:3000/"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [duct/core "0.6.2"]
                 [duct/module.logging "0.3.1"]
                 [duct/module.web "0.6.4"]
                 [duct/module.ataraxy "0.2.0"]
                 [duct/module.sql "0.4.2"]
                 [duct/handler.sql "0.3.1"]
                 [duct/database.sql.hikaricp "0.3.3"]
                 [hiccup "1.0.5"]
                 ; alternate servers
                 [duct/server.http.http-kit "0.1.2"]
                 [duct/server.http.aleph "0.1.2"]
                 [me.grison/duct-immutant "0.1.0"]
                 ; tested databases
                 [org.postgresql/postgresql "42.2.5"]
                 [me.grison/duct-mongodb "0.1.1"]]
  :plugins [[duct/lein-duct "0.10.6"]]
  :main ^:skip-aot hello.main
  :resource-paths ["resources" "target/resources"]
  :prep-tasks     ["javac" "compile" ["run" ":duct/compiler"]]
  :profiles
  {:dev  [:project/dev :profiles/dev]
   :repl {:prep-tasks   ^:replace ["javac" "compile"]
          :repl-options {:init-ns user}}
   :uberjar {:aot :all}
   :profiles/dev {}
   :project/dev  {:source-paths   ["dev/src"]
                  :resource-paths ["dev/resources"]
                  :dependencies   [[integrant/repl "0.2.0"]
                                   [eftest "0.4.1"]
                                   [kerodon "0.9.0"]]}})
