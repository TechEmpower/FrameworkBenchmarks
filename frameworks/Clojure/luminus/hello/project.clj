(defproject hello "luminus"

  :description "Luminus framework benchmarks"
  :url "https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Clojure/luminus"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [ring-server "0.4.0"]
                 [selmer "0.8.2"]
                 [com.taoensso/timbre "3.4.0"]
                 [com.taoensso/tower "3.0.2"]
                 [markdown-clj "0.9.65"]
                 [environ "1.0.0"]
                 [im.chit/cronj "1.4.3"]
                 [compojure "1.3.3"]
                 [ring/ring-defaults "0.1.4"]
                 [ring/ring-session-timeout "0.1.0"]
                 [ring-middleware-format "0.5.0"]
                 [noir-exception "0.2.3"]
                 [bouncer "0.3.2"]
                 [prone "0.8.1"]
                 [org.clojure/tools.nrepl "0.2.8"]
                 [yesql "0.5.0-rc2"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [http-kit "2.1.19"]
                 [org.clojure/tools.cli "0.2.1"]]

  :min-lein-version "2.0.0"
  :uberjar-name "hello.jar"
  :jvm-opts ["-server"]

  :main hello.core

  :plugins [[lein-ring "0.9.1"]
            [lein-environ "1.0.0"]]

  :profiles
  {:uberjar {:omit-source true
             :env {:production true}
             :aot :all}
   :dev {:dependencies [[ring-mock "0.1.5"]
                        [ring/ring-devel "1.3.2"]
                        [pjstadig/humane-test-output "0.7.0"]]
         :source-paths ["env/dev/clj"]

         :repl-options {:init-ns hello.repl}
         :injections [(require 'pjstadig.humane-test-output)
                      (pjstadig.humane-test-output/activate!)]
         :env {:dev true}}})
