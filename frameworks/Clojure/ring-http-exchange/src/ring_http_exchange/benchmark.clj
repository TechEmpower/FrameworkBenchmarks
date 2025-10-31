(ns ring-http-exchange.benchmark
  (:gen-class)
  (:require
    [jj.majavat :as majavat]
    [jj.majavat.renderer :refer [->StringRenderer]]
    [jj.majavat.renderer.sanitizer :refer [->Html]]
    [jj.sql.boa :as boa]
    [jsonista.core :as json]
    [next.jdbc.connection :as connection]
    [ring-http-exchange.core :as server])
  (:import
    (com.zaxxer.hikari HikariDataSource)
    (io.netty.channel.epoll EpollEventLoopGroup)))

(def query-fortunes (boa/execute (boa/->NextJdbcAdapter) "fortune.sql"))

(def db-spec {:auto-commit        true
              :read-only          false
              :connection-timeout 30000
              :validation-timeout 5000
              :idle-timeout       600000
              :max-lifetime       1800000
              :minimum-idle       10
              :maximum-pool-size  520
              :minimum-pool-size  512
              :register-mbeans    false
              :jdbcUrl            "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"})

(def ^:private ^:const additional-message {:id      0
                                           :message "Additional fortune added at request time."})
(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})
(def ^:private ^:const json-headers {"Server"       "ring-http-exchange"
                                     "Content-Type" "application/json"})
(def ^:private ^:const plaintext-response {:status  200
                                           :headers {"Server"       "ring-http-exchange"
                                                     "Content-Type" "text/plain"}
                                           :body    "Hello, World!"})
(def ^:private render-fortune (majavat/build-renderer "fortune.html"
                                                      {:renderer (->StringRenderer
                                                                   {:sanitizer (->Html)})}))

(defn- get-body [datasource]
  (let [context (as-> (query-fortunes datasource) fortunes
                      (conj fortunes additional-message)
                      (sort-by :message fortunes))]
    (render-fortune {:messages context})))

(defn -main
  [& _]
  (println "Starting server on port 8080")
  (let [datasource (connection/->pool HikariDataSource db-spec)]
    (server/run-http-server
      (fn [req]
        (case (req :uri)
          "/json" {:status  200
                   :headers json-headers
                   :body    (json/write-value-as-bytes {:message "Hello, World!"})}
          "/fortunes" (let [body (get-body datasource)]
                        {:status  200
                         :headers fortune-headers
                         :body    body})
          plaintext-response))
      {:port     8080
       :host     "0.0.0.0"
       :executor (EpollEventLoopGroup.)})))