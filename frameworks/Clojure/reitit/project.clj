(defproject hello "reitit"
  :description "Immutant-nio, Reitit, Jsonista & Porsas"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [ikitommi/immutant-web "3.0.0-alpha4"]
                 [metosin/reitit "0.3.9"]
                 [metosin/jsonista "0.2.3"]
                 [metosin/porsas "0.0.1-alpha10"]
                 [hikari-cp "2.7.1"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
