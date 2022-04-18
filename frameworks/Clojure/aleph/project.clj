(defproject hello "aleph"
  :description "JSON/plaintext tests"
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [org.clojure/tools.cli "0.3.7"]
                 [aleph "0.4.7"]
                 [metosin/jsonista "0.3.5"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
