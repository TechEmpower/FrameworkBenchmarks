(ns hello.handler
  (:require [immutant.web :as web]
            [hikari-cp.core :as hikari]
            [reitit.ring :as ring]
            [porsas.core :as p]
            [porsas.async :as pa]
            [jsonista.core :as j])
  (:import (java.util.concurrent ThreadLocalRandom))
  (:gen-class))

(defn random []
  (unchecked-inc (.nextInt (ThreadLocalRandom/current) 10000)))

(defn plain-text-handler [_]
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body (.getBytes "Hello, World!")})

(defn json-handler [_]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (j/write-value-as-bytes {:message "Hello, World!"})})

(defn sync-db-handler [pool]
  (let [mapper (p/compile {:row (p/rs->compiled-record)})]
    (fn [_]
      (let [world (with-open [con (p/get-connection pool)]
                    (p/query-one mapper con ["SELECT id, randomnumber from WORLD where id=?" (random)]))]
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body (j/write-value-as-bytes world)}))))

(defn async-db-handler [pool]
  (let [mapper (pa/data-mapper {:row (pa/rs->compiled-record)})]
    (fn [_ respond _]
      (pa/query-one
        mapper
        pool
        ["SELECT id, randomnumber from WORLD where id=$1" (random)]
        (fn [world]
          (respond
            {:status 200
             :headers {"Content-Type" "application/json"}
             :body (j/write-value-as-bytes world)}))))))

(defn -main [& [async?]]
  (let [cpus (.availableProcessors (Runtime/getRuntime))
        pool (if async?
               (pa/pool
                 {:uri "postgresql://tfb-database:5432/hello_world"
                  :user "benchmarkdbuser"
                  :password "benchmarkdbpass"
                  :size 64})
               (hikari/make-datasource
                 {:jdbc-url "jdbc:postgresql://tfb-database:5432/hello_world"
                  :username "benchmarkdbuser"
                  :password "benchmarkdbpass"
                  :maximum-pool-size 256}))
        db-handler (if async?
                     (web/async (async-db-handler pool))
                     (web/dispatch (sync-db-handler pool)))]
    (web/run
      (ring/ring-handler
        (ring/router
          [["/plaintext" (web/constantly plain-text-handler)]
           ["/json" json-handler]
           ["/db" db-handler]])
        (ring/create-default-handler)
        {:inject-match? false
         :inject-router? false})
      {:port 8080
       :host "0.0.0.0"
       :dispatch? false
       :io-threads (* 2 cpus)
       :worker-threads 256
       :server {:always-set-keep-alive false}})))
