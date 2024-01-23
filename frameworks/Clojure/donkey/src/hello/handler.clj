(ns hello.handler
  (:require [com.appsflyer.donkey.core :as donkey-core]
            [com.appsflyer.donkey.server :as donkey-server]
            [com.appsflyer.donkey.middleware.json :as json])
  (:gen-class)
  (:import (io.vertx.core.impl.cpu CpuCoreSensor)))

(defn- json-handler [_ res _]
  (res {:status  200
        :headers {"content-type" "application/json"}
        :body    {"message" "Hello, World!"}}))

(def ^:private json-route {:methods    [:get]
                           :middleware [(json/make-serialize-middleware)]
                           :path       "/json"
                           :handler    json-handler})

(def ^:private hello-world-body
  (bytes (byte-array (map (comp byte int) "Hello, World!"))))

(def ^:private hello-world-response
  {:status  200
   :headers {"content-type" "text/plain"}
   :body    hello-world-body})

(defn- hello-world-handler [_ res _]
  (res hello-world-response))

(def ^:private hello-world-route {:methods [:get]
                                  :path    "/plaintext"
                                  :handler hello-world-handler})

(defn -main [& _]
  (let [concurrency (max 1 (- (CpuCoreSensor/availableProcessors) 1))]
    (->
      (donkey-core/create-donkey
        {:event-loops concurrency})
      (donkey-core/create-server {:port           8080
                                  :routes         [hello-world-route json-route]
                                  :instances      concurrency
                                  :compression    false
                                  :decompression  false
                                  :accept-backlog 20000
                                  :date-header    true
                                  :server-header  true
                                  :keep-alive     true})
      (donkey-server/start-sync))))
