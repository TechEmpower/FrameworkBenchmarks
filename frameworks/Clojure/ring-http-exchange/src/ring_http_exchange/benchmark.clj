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
    (java.util.concurrent Executors)))

(def query-fortunes (boa/execute (boa/->NextJdbcAdapter) "fortune.sql"))

(def db-spec {:auto-commit        false
              :connection-timeout 3000
              :validation-timeout 1000
              :idle-timeout       300000
              :max-lifetime       1800000
              :minimum-idle       128
              :maximum-pool-size  1024
              :register-mbeans    false
              :jdbcUrl            "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass&prepareThreshold=1"})

(def ^:private ^:const additional-message {:id      0
                                           :message "Additional fortune added at request time."})
(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})
(def ^:private ^:const json-headers {"Server"       "ring-http-exchange"
                                     "Content-Type" "application/json"})

(def ^:private render-fortune (majavat/build-renderer "fortune.html"
                                                      {:renderer (->StringRenderer
                                                                   {:sanitizer (->Html)})}))


(defn- plaintext-response []
  {:status  200
   :headers {"Server"       "ring-http-exchange"
             "Content-Type" "text/plain"}
   :body    "Hello, World!"})

(defn- get-body [datasource]
  (let [context (as-> (query-fortunes datasource) fortunes
                      (conj fortunes additional-message)
                      (sort-by :message fortunes))]
    (render-fortune {:messages context})))

(defn -main
  [& _]
  (System/setProperty "jdk.virtualThreadScheduler.parallelism" 
                    (str (* 4 (.availableProcessors (Runtime/getRuntime)))))

  (println "Starting server on port 8080")
  (let [datasource (connection/->pool HikariDataSource db-spec)]
    (server/run-http-server
      (fn [req]
        (case (req :uri)
          "/plaintext" (plaintext-response)
          "/json" {:status  200
                   :headers json-headers
                   :body    (json/write-value-as-bytes {:message "Hello, World!"})}
          "/fortunes" (let [body (get-body datasource)]
                        {:status  200
                         :headers fortune-headers
                         :body    body})
          (plaintext-response)))
      {:port     8080
       :host     "0.0.0.0"
       :executor (Executors/newVirtualThreadPerTaskExecutor)})))