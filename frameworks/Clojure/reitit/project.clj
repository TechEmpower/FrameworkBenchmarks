(defproject hello "reitit"
  :description "pohjavirta, reitit, jsonista & porsas"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [metosin/pohjavirta "0.0.1-alpha5"]
                 [metosin/porsas "0.0.1-alpha13"]
                 [metosin/jsonista "0.2.5"]
                 [metosin/reitit "0.3.10"]
                 [hikari-cp "2.9.0"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
