(defproject hello "aleph"
  :description "JSON/plaintext tests"
  :dependencies [[org.clojure/clojure "1.7.0-beta2"]
                 [clj-tuple "0.2.1"]
                 [org.clojure/tools.cli "0.3.1"]
                 [aleph "0.4.0"]
                 [cheshire "5.4.0"]]
  :main hello.handler
  :aot :all)
