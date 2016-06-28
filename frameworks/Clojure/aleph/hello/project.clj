(defproject hello "aleph"
  :description "JSON/plaintext tests"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-tuple "0.2.2"]
                 [org.clojure/tools.cli "0.3.3"]
                 [aleph "0.4.1-beta2"]
                 [cheshire "5.5.0"]]
  :main hello.handler
  :aot :all)
