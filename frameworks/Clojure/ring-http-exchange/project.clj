(defproject ring-http-server "1.0.0"
  :description "ring benchmark"
  :url ""
  :license {:name "EPL-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.12.4"]
                 [org.clojure/tools.logging "1.3.0"]
                 [org.clojars.jj/ring-http-exchange "1.4.1"]
                 [seancorfield/next.jdbc "1.2.659"]
                 [org.clojars.jj/majavat "1.20.1"]
                 [org.clojars.jj/boa-sql "1.0.8"]
                 [org.clojars.jj/async-boa-sql "1.0.8"]
                 [hikari-cp "3.3.0"]
                 [org.clojars.jj/arminio "1.0.0"]
                 [org.clojars.jj/vertx-pg-client-async-boa-adapter "1.0.1"]
                 [org.postgresql/postgresql "42.7.8"]
                 [metosin/jsonista "0.3.13"]
                 ]

  :profiles {:robaho {:dependencies [[io.github.robaho/httpserver "1.0.29"]]}}
  :resource-paths ["resources"]
  :main ring-http-exchange.benchmark)
