(defproject hello "reitit"
  :description "Immutant, Reitit, Jsonista"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [ikitommi/immutant-web "3.0.0-alpha1"]
                 [metosin/jsonista "0.1.1"]
                 [metosin/reitit "0.1.0-20180124.072356-20"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
