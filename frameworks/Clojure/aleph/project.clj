(defproject hello "aleph"
  :description "JSON/plaintext tests"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clj-tuple "0.2.2"]
                 [org.clojure/tools.cli "0.3.7"]
                 [aleph "0.4.5-alpha6"]
                 [javax.xml.bind/jaxb-api "2.3.0"]
                 [metosin/jsonista "0.2.0"]]
  :main hello.handler
  :aot :all)
