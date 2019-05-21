(defproject hello "reitit"
  :description "Immutant-nio, Reitit, Jsonista & Porsas"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ikitommi/immutant-web "3.0.0-alpha2"]
                 [metosin/jsonista "0.2.2"]
                 [hikari-cp "2.7.1"]
                 [org.postgresql/postgresql "42.2.5"]
                 [metosin/porsas "0.0.1-alpha4"]
                 [metosin/reitit "0.3.3"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
