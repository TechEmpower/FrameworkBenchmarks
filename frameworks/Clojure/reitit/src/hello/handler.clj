(ns hello.handler
  (:require [immutant.web :as web]
            [hikari-cp.core :as hikari]
            [reitit.ring :as ring]
            [immutant.web.internal.ring :as immutant]
            [porsas.core :as p]
            [jsonista.core :as j])
  (:gen-class)
  (:import (java.util.concurrent ThreadLocalRandom)
           (io.undertow.server HttpServerExchange)))

(defn blocking [handler]
  (fn [req]
    (let [exchange ^HttpServerExchange (:server-exchange req)]
      (if (.isInIoThread exchange)
        (.dispatch exchange ^Runnable ^:once (fn []
                                               (.startBlocking exchange)
                                               (immutant/write-response exchange (handler req))
                                               (.endExchange exchange)))
        (handler req)))))

(defn random []
  (unchecked-inc (.nextInt (ThreadLocalRandom/current) 10000)))

(def query-one (:query-one (p/compile {:row (p/rs->compiled-record)})))

(defn random-world [ds]
  (with-open [con (p/get-connection ds)]
    (query-one con ["SELECT id, randomnumber from WORLD where id=?" (random)])))

(defn plain-text-handler [_]
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body (.getBytes "Hello, World!")})

(defn json-handler [_]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (j/write-value-as-bytes {:message "Hello, World!"})})

(defn db-handler [ds]
  (fn [_]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (j/write-value-as-bytes (random-world ds))}))

(defn -main [& _]
  (let [ds (hikari/make-datasource
             {:read-only true
              :maximum-pool-size 48
              :pool-name "db-pool"
              :adapter "postgresql"
              :username "benchmarkdbuser"
              :password "benchmarkdbpass"
              :database-name "hello_world"
              :server-name "tfb-database"
              :port-number 5432
              :register-mbeans false})]
    (web/run
      (ring/ring-handler
        (ring/router
          [["/plaintext" plain-text-handler]
           ["/json" json-handler]
           ["/db" (blocking (db-handler ds))]])
        (ring/create-default-handler)
        {:inject-match? false
         :inject-router? false})
      {:port 8080
       :host "0.0.0.0"
       :io-threads (* 2 (.availableProcessors (Runtime/getRuntime)))
       :dispatch? false
       :worker-threads 48
       :server {:always-set-keep-alive false}})))

(comment
  (-main))
