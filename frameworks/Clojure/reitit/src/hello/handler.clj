(ns hello.handler
  (:require [immutant.web :as web]
            [reitit.ring :as ring]
            [jsonista.core :as j])
  (:gen-class))

(defn json-handler [_]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (j/write-value-as-bytes {:message "Hello, World!"})})

(def app
  (ring/ring-handler
    (ring/router ["/json" json-handler])
    (ring/create-default-handler)))

(defn -main [& _]
  (web/run
    app
    {:port 8080
     :host "0.0.0.0"
     :dispatch? false
     :server {:always-set-keep-alive false}}))