(ns ring-http-exchange.benchmark
  (:gen-class)
  (:require
    [next.jdbc.connection :as connection]
    [ring-http-exchange.input-stream-handler :as input-stream-handler]
    [ring-http-exchange.core :as server]
    [ring-http-exchange.string-handler :as string-handler])
  (:import
    (com.zaxxer.hikari HikariDataSource)
    (java.util.concurrent Executors)))

(def db-spec {:idle-timeout      15000
              :max-lifetime      60000
              :minimum-idle      0
              :maximum-pool-size 1024
              :jdbcUrl           "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"})

(defn -main
  [& args]
  (println "Starting server on port 8080")
  (let [datasource (connection/->pool HikariDataSource db-spec)
        use-inputstream? (some #{"--inputstream" "-i" "inputstream"} args)]
    (.addDataSourceProperty datasource "tcpKeepAlive" "true")
    (.addDataSourceProperty datasource "useSSL" false)
    (.addDataSourceProperty datasource "prepStmtCacheSize" "250")
    (.addDataSourceProperty datasource "cachePrepStmts" "true")
    (.addDataSourceProperty datasource "prepStmtCacheSqlLimit" "2048")
    (server/run-http-server
      (if use-inputstream?
        (input-stream-handler/get-handler datasource)
        (string-handler/get-handler datasource))
      {:port     8080
       :host     "0.0.0.0"
       :executor (Executors/newCachedThreadPool)})))