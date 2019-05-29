(ns hello.handler
  (:require [immutant.web :as web]
            [hikari-cp.core :as hikari]
            [reitit.ring :as ring]
            [porsas.core :as p]
            [jsonista.core :as j])
  (:import (java.util.concurrent ThreadLocalRandom))
  (:gen-class))

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
             {:jdbc-url "jdbc:postgresql://tfb-database:5432/hello_world"
              :username "benchmarkdbuser"
              :password "benchmarkdbpass"
              :maximum-pool-size 256})]
    (web/run
      (ring/ring-handler
        (ring/router
          [["/plaintext" (web/constantly plain-text-handler)]
           ["/json" json-handler]
           ["/db" (web/dispatch (db-handler ds))]])
        (ring/create-default-handler)
        {:inject-match? false
         :inject-router? false})
      {:port 8080
       :host "0.0.0.0"
       :dispatch? false
       :io-threads (* 2 (.availableProcessors (Runtime/getRuntime)))
       :worker-threads (* 8 (.availableProcessors (Runtime/getRuntime)))
       :server {:always-set-keep-alive false}})))
