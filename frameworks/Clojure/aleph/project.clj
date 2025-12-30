(defproject hello "aleph"
  :description "Aleph benchmarks"
  :dependencies [[org.clojure/clojure "1.12.3"]
                 [aleph "0.9.3"]
                 [metosin/jsonista "0.3.13"]
                 [hiccup "2.0.0"]
                 [seancorfield/next.jdbc "1.2.659"]
                 [hikari-cp "3.3.0"]
                 [org.postgresql/postgresql "42.7.8"]
                 ]
  :main hello.handler
  :aot :all)
