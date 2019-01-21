(defproject hello "reitit"
  :description "Immutant, Reitit, Jsonista"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ikitommi/immutant-web "3.0.0-alpha1"]
                 [metosin/jsonista "0.2.2"]
                 [metosin/reitit "0.2.12"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
