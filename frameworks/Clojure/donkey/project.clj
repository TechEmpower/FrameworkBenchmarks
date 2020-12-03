(defproject hello "donkey-SNAPSHOT"
  :description "Donkey Server"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.appsflyer/donkey "0.1.0-SNAPSHOT"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :main hello.handler
  :aot :all)
