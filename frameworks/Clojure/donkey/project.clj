(defproject hello "donkey"
  :description "Donkey Server"
  :dependencies [[org.clojure/clojure "1.12.3"]
                 [com.appsflyer/donkey "0.5.2"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
