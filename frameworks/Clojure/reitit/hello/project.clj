(defproject hello "reitit"
  :description "Immutant, Reitit, Jsonista"
  :dependencies [[org.clojure/clojure "1.9.0-RC1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.immutant/web "2.1.9"]
                 [metosin/jsonista "0.1.0-20171106.055332-4"]
                 [metosin/reitit "0.1.0-20171101.170818-9"]]
  :main hello.handler
  :aot :all)
