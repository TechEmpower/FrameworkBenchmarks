(defproject hello "reitit"
  :description "pohjavirta, reitit, jsonista & porsas"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [metosin/pohjavirta "0.0.1-alpha1"]
                 [metosin/porsas "0.0.1-alpha12"]
                 [metosin/jsonista "0.2.3"]
                 [metosin/reitit "0.3.9"]
                 [hikari-cp "2.7.1"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
