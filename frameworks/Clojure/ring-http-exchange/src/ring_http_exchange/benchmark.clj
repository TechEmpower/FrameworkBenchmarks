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

(defrecord Response [body status headers])

(def query-fortunes (boa/execute (boa/->NextJdbcAdapter) "fortune.sql"))

(def db-spec {:auto-commit        false
              :connection-timeout 1000
              :validation-timeout 1000
              :idle-timeout       15000
              :max-lifetime       60000
              :minimum-idle       0
              :maximum-pool-size  128
              :register-mbeans    false
              :jdbcUrl            "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass&prepareThreshold=1"}
  )

(def ^:private ^:const hello-world "Hello, World!")
(def ^:private ^:const additional-message {:id      0
                                           :message "Additional fortune added at request time."})
(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})
(def ^:private ^:const json-headers {"Server"       "ring-http-exchange"
                                     "Content-Type" "application/json"})
(def ^:private ^:const plain-text-headers {"Server"       "ring-http-exchange"
                                           "Content-Type" "text/plain"})

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
          "/plaintext" (Response. hello-world 200 plain-text-headers)
          "/json" (Response. (json/write-value-as-string {:message hello-world}) 200 json-headers)
          "/fortunes" (Response. (get-body datasource) 200 fortune-headers)
          (Response. hello-world 200 {"Server"       "ring-http-exchange"
                                      "Content-Type" "text/plain"})))
      {:port     8080
       :host     "0.0.0.0"
       :executor (Executors/newVirtualThreadPerTaskExecutor)})))