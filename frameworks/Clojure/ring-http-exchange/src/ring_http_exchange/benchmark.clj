(ns ring-http-exchange.benchmark
    (:gen-class)
    (:require [jsonista.core :as json]
      [ring-http-exchange.core :as server])
    (:import
      (java.util.concurrent Executors)))

(def ^:private ^:const json-headers {"Server"       "ring-http-exchange"
                                     "Content-Type" "application/json"})
(def ^:private ^:const plaintext-response
  {:status  200
   :headers {
             "Server"       "ring-http-exchange"
             "Content-Type" "text/plain"}
   :body    "Hello, World!"})

(defn -main
      [& args]
      (println "Starting server on port 8080")
      (server/run-http-server
        (fn [req]
            (case (req :uri)
                  "/json" {:status  200
                           :headers json-headers
                           :body    (json/write-value-as-bytes {:message "Hello, World!"})}
                  plaintext-response))
        {:port     8080
         :host     "0.0.0.0"
         :executor (Executors/newVirtualThreadPerTaskExecutor)}))
