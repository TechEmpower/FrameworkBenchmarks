(ns ring-http-exchange.benchmark
  (:gen-class)
  (:require
    [jsonista.core :as json]
    [jj.majavat :as majavat]
    [jj.majavat.renderer :refer [->InputStreamRenderer]]
    [jj.majavat.renderer.sanitizer :refer [->Html]]
    [ring-http-exchange.core :as server]
    [next.jdbc :as jdbc]
    [next.jdbc.connection :as connection])
  (:import
    (com.zaxxer.hikari HikariDataSource)
    (java.util.concurrent Executors)))

(def db-spec
  {:jdbcUrl "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"})

(def datasource
  (connection/->pool HikariDataSource db-spec))

(defn query-fortunes []
  (jdbc/execute! datasource
                 ["SELECT * FROM \"Fortune\""]
                 {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))

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
                                                      {:renderer (->InputStreamRenderer
                                                                   {:sanitizer (->Html)})}))

(defn -main
  [& _]
  (println "Starting server on port 8080")
  (server/run-http-server
    (fn [req]
      (case (req :uri)
        "/json" {:status  200
                 :headers json-headers
                 :body    (json/write-value-as-bytes {:message "Hello, World!"})}
        "/fortunes" (let [input (as-> (query-fortunes) fortunes
                                      (conj fortunes additional-message)
                                      (sort-by :message fortunes))
                          body (render-fortune {:messages input})]
                      {:status  200
                       :headers fortune-headers
                       :body    body})
        plaintext-response))
    {:port     8080
     :host     "0.0.0.0"
     :executor (Executors/newVirtualThreadPerTaskExecutor)}))