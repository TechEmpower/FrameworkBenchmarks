(defproject hello "immutant"
  :description "Immutant, Jsonista"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [ikitommi/immutant-web "3.0.0-alpha1"]
                 [metosin/jsonista "0.2.0"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
