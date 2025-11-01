(defproject hello "0.1.0-SNAPSHOT"

  :description "TechEmpower Luminus benchmark"
  :url "https://github.com/TechEmpower/FrameworkBenchmarks"

  :dependencies [[org.clojure/clojure "1.12.3"]
                 [cheshire "6.1.0"]
                 [selmer "1.12.67"]
                 [metosin/ring-http-response "0.9.5"]
                 [org.webjars/bootstrap "5.3.8"]
                 [org.clojure/tools.logging "1.3.0"]
                 [compojure "1.7.2"]
                 [mount "0.1.23"]
                 [cprop "0.1.21"]
                 [org.clojure/tools.cli "1.2.245"]
                 [luminus-immutant "0.2.5"]
                 [luminus-migrations "0.7.5"]
                 [conman "0.9.6"]
                 [org.postgresql/postgresql "42.7.8"]
                 [luminus-log4j "0.1.7"]]

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
