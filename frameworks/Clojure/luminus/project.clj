(defproject hello "0.1.0-SNAPSHOT"

  :description "TechEmpower Luminus benchmark"
  :url "https://github.com/TechEmpower/FrameworkBenchmarks"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [cheshire "5.7.0"]
                 [selmer "1.10.7"]
                 [metosin/ring-http-response "0.8.2"]
                 [org.webjars/bootstrap "4.2.1"]
                 [org.clojure/tools.logging "0.4.1"]
                 [compojure "1.5.2"]
                 [mount "0.1.11"]
                 [cprop "0.1.10"]
                 [org.clojure/tools.cli "0.3.5"]
                 [luminus-immutant "0.2.3"]
                 [luminus-migrations "0.3.0"]
                 [conman "0.6.3"]
                 [org.postgresql/postgresql "42.2.5"]
                 [luminus-log4j "0.1.5"]]

  :min-lein-version "2.0.0"

  :jvm-opts ["-server" "-Dconf=.lein-env"]
  :source-paths ["src/clj"]
  :resource-paths ["resources"]

  :main hello.core
  :migratus {:store :database :db ~(get (System/getenv) "DATABASE_URL")}

  :profiles
  {:uberjar {:omit-source true
             :jvm-opts ["-D\"clojure.compiler.direct-linking=true\""]
             :aot :all
             :uberjar-name "hello.jar"
             :source-paths ["env/prod/clj"]
             :resource-paths ["env/prod/resources"]}
   :dev           [:project/dev :profiles/dev]
   :test          [:project/test :profiles/test]
   :project/dev  {:dependencies [[prone "1.1.4"]
                                 [ring/ring-mock "0.3.0"]
                                 [ring/ring-devel "1.5.1"]
                                 [pjstadig/humane-test-output "0.8.1"]
                                 [mvxcvi/puget "1.0.1"]]


                  :source-paths ["env/dev/clj" "test/clj"]
                  :resource-paths ["env/dev/resources"]
                  :repl-options {:init-ns user}
                  :injections [(require 'pjstadig.humane-test-output)
                               (pjstadig.humane-test-output/activate!)]}
   :project/test {:resource-paths ["env/dev/resources" "env/test/resources"]}
   :profiles/dev {}
   :profiles/test {}})
