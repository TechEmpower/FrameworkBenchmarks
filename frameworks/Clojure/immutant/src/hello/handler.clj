(ns hello.handler
  (:require [immutant.web :as web]
            [jsonista.core :as json])
  (:gen-class))

(def plaintext-response
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body (.getBytes "Hello, World!")})

(defn app [req]
  (let [uri (:uri req)]
    (cond
      (.equals "/plaintext" uri) plaintext-response
      (.equals "/json" uri) {:status 200
                             :headers {"content-type" "application/json"}
                             :body (json/write-value-as-bytes {:message "Hello, World!"})}
      :else {:status 404})))

(defn -main [& _]
  (web/run
    app
    {:port 8080
     :host "0.0.0.0"
     :dispatch? false
     :server {:always-set-keep-alive false}}))