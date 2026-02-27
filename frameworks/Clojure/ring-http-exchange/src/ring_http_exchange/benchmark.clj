(ns ring-http-exchange.benchmark
  (:gen-class)
  (:require
    [next.jdbc.connection :as connection]
    [ring-http-exchange.input-stream-handler :as input-stream-handler]
    [ring-http-exchange.core :as server]
    [ring-http-exchange.string-handler :as string-handler])
  (:import
    (com.zaxxer.hikari HikariDataSource)
    (java.util.concurrent Executors)
    (jj.arminio.concurrent ProxyExecutorService)))

(def db-spec {:idle-timeout      15000
              :max-lifetime      60000
              :minimum-idle      (* 4 (.availableProcessors (Runtime/getRuntime)))
              :maximum-pool-size (* 4 (.availableProcessors (Runtime/getRuntime)))
              :jdbcUrl           "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass&tlsnowait=true"})

(defn -main
  [& args]
  (println "Starting server on port 8080")
  (let [default-executor-service (ProxyExecutorService. (Executors/newCachedThreadPool))
        default-server-config {:port              8080
                               :host              "0.0.0.0"
                               :lazy-request-map? true
                               :executor          default-executor-service}

        datasource (connection/->pool HikariDataSource db-spec)
        use-inputstream? (some #{"--inputstream"} args)
        async? (some #{"--async"} args)
        ]
    (.addDataSourceProperty datasource "tcpKeepAlive" "true")
    (.addDataSourceProperty datasource "useSSL" false)
    (.addDataSourceProperty datasource "prepStmtCacheSize" "250")
    (.addDataSourceProperty datasource "cachePrepStmts" "true")
    (.addDataSourceProperty datasource "prepStmtCacheSqlLimit" "2048")

    (let [handler (cond
                    async? (string-handler/get-async-handler datasource)
                    use-inputstream? (input-stream-handler/get-handler datasource)
                    :else (string-handler/get-handler datasource))
          config (cond-> default-server-config
                         async? (assoc :async? true))]
      (server/run-http-server handler config))))
