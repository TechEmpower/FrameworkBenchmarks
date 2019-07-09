(ns hello.handler
  (:require [pohjavirta.server :as server]
            [pohjavirta.async :as a]
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
   :headers {"Content-Type" "text/plain"}
   :body "Hello, World!"})

(defn json-handler [_]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (j/write-value-as-bytes {:message "Hello, World!"})})

(defn sync-db-handler [mapper pool]
  (fn [_]
    (let [world (with-open [con (p/get-connection pool)]
                  (p/query-one mapper con ["SELECT id, randomnumber from WORLD where id=?" (random)]))]
      {:status 200
       :headers {"Content-Type" "application/json"}
       :body (j/write-value-as-bytes world)})))

(defn async-db-handler [mapper pool]
  (fn [_]
    (-> (pa/query-one mapper pool ["SELECT id, randomnumber from WORLD where id=$1" (random)])
        (pa/then (fn [world]
                   {:status 200
                    :headers {"Content-Type" "application/json"}
                    :body (j/write-value-as-bytes world)})))))

(defn -main [& [mode ?size]]
  (let [cpus (.availableProcessors (Runtime/getRuntime))
        size (try (Integer/parseInt ?size) (catch Exception _ (* 2 cpus)))
        db-handler (cond 
                     ;; reactive pg-client 
                     (= mode "async")
                     (async-db-handler
                       (pa/data-mapper {:row (pa/rs->compiled-record)})
                       (pa/pool
                         {:uri "postgresql://tfb-database:5432/hello_world"
                          :user "benchmarkdbuser"
                          :password "benchmarkdbpass"
                          :size size}))
                     ;; jdbc
                     (= mode "sync")
                     (sync-db-handler
                       (p/data-mapper {:row (p/rs->compiled-record)})
                       (hikari/make-datasource
                         {:jdbc-url "jdbc:postgresql://tfb-database:5432/hello_world"
                          :username "benchmarkdbuser"
                          :password "benchmarkdbpass"
                          :maximum-pool-size size}))
                     ;; none
                     :else (constantly nil))]
    (println "Starting" mode "server, with pool-size" size)
    (-> (ring/ring-handler
          (ring/router
            [["/plaintext" (server/constantly plain-text-handler)]
             ["/json" json-handler]
             ["/db" db-handler]])
          (ring/create-default-handler)
          {:inject-match? false
           :inject-router? false})
        (server/create
          {:port 8080
           :host "0.0.0.0"
           :io-threads (* 2 cpus)
           :worker-threads 256})
        (server/start))))
