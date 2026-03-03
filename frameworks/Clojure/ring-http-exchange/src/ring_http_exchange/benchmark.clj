(ns ring-http-exchange.benchmark
  (:gen-class)
  (:require
    [next.jdbc.connection :as connection]
    [ring-http-exchange.input-stream-handler :as input-stream-handler]
    [ring-http-exchange.core :as server]
    [ring-http-exchange.string-handler :as string-handler])
  (:import
    (com.zaxxer.hikari HikariDataSource)
    (io.vertx.pgclient PgBuilder PgConnectOptions SslMode)
    (io.vertx.sqlclient PoolOptions)
    (java.util.concurrent Executors)
    (jj.arminio.concurrent ProxyExecutorService)))

(def db-spec {:idle-timeout      15000
              :max-lifetime      60000
              :minimum-idle      520
              :maximum-pool-size 1024
              :jdbcUrl           "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass&tlsnowait=true"})



(defn create-vertx-pool []
  (let [connect-opts (-> (PgConnectOptions.)
                         (.setHost "tfb-database")
                         (.setPort 5432)
                         (.setDatabase "hello_world")
                         (.setUser "benchmarkdbuser")
                         (.setPassword "benchmarkdbpass")
                         (.setSslMode SslMode/DISABLE)
                         (.setCachePreparedStatements true)
                         (.setPreparedStatementCacheMaxSize 256))
        pool-opts (-> (PoolOptions.)
                      (.setMaxSize 512))]
    (-> (PgBuilder/pool)
        (.connectingTo connect-opts)
        (.with pool-opts)
        (.build))))

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
        vertx? (some #{"--vertx"} args)
        ]
    (.addDataSourceProperty datasource "tcpKeepAlive" "true")
    (.addDataSourceProperty datasource "useSSL" false)
    (.addDataSourceProperty datasource "prepStmtCacheSize" "250")
    (.addDataSourceProperty datasource "cachePrepStmts" "true")
    (.addDataSourceProperty datasource "prepStmtCacheSqlLimit" "2048")

    (let [handler (cond
                    vertx? (string-handler/get-vertx-handler (create-vertx-pool))
                    async? (string-handler/get-async-handler datasource)
                    use-inputstream? (input-stream-handler/get-handler datasource)
                    :else (string-handler/get-handler datasource))
          config (cond-> default-server-config
                         async? (assoc :async? true))]
      (server/run-http-server handler config))))
