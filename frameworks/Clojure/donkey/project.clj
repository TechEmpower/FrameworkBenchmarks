(defproject hello "donkey"
  :description "Donkey Server"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.appsflyer/donkey "0.1.0"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
