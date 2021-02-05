(defproject hello "donkey"
  :description "Donkey Server"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.appsflyer/donkey "0.4.1"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
