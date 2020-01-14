(ns hello.handler
  (:require [pohjavirta.server :as server]
            [pohjavirta.exchange :as exchange]
            [hikari-cp.core :as hikari]
            [reitit.ring :as ring]
            [porsas.jdbc :as jdbc]
            [porsas.async :as async]
            [jsonista.core :as j])
  (:import (java.util.concurrent ThreadLocalRandom)
           (java.util.function Supplier)
           (clojure.lang IDeref))
  (:gen-class))

;;
;; utils
;;

(defn random []
  (unchecked-inc (.nextInt (ThreadLocalRandom/current) 10000)))

(defmacro thread-local [& body]
  `(let [tl# (ThreadLocal/withInitial (reify Supplier (get [_] ~@body)))]
     (reify IDeref (deref [_] (.get tl#)))))

;;
;; handlers
;;

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
    (let [world (with-open [con (jdbc/get-connection pool)]
                  (jdbc/query-one mapper con ["SELECT id, randomnumber from WORLD where id=?" (random)]))]
      {:status 200
       :headers {"Content-Type" "application/json"}
       :body (j/write-value-as-bytes world)})))

(defn async-db-handler [mapper pool-ref]
  (fn [_]
    (-> (async/query-one mapper @pool-ref ["SELECT id, randomnumber from WORLD where id=$1" (random)])
        (async/then (fn [world]
                      {:status 200
                       :headers {"Content-Type" "application/json"}
                       :body (j/write-value-as-bytes world)})))))

;;
;; server
;;

(defn -main [& [mode]]
  (let [cpus (.availableProcessors (Runtime/getRuntime))
        db-handler (cond
                     ;; reactive pg-client in NIO-pool
                     (= mode "async")
                     (async-db-handler
                       (async/context {:row (async/rs->compiled-record)})
                       ;; thread local pool provider
                       (thread-local
                         (async/pool
                           {:uri "postgresql://tfb-database:5432/hello_world"
                            :user "benchmarkdbuser"
                            :password "benchmarkdbpass"
                            :size 1})))
                     ;; jdbc in worker-pool
                     (= mode "sync")
                     (exchange/dispatch
                       (sync-db-handler
                         (jdbc/context {:row (jdbc/rs->compiled-record)})
                         (hikari/make-datasource
                           {:jdbc-url "jdbc:postgresql://tfb-database:5432/hello_world"
                            :username "benchmarkdbuser"
                            :password "benchmarkdbpass"
                            :maximum-pool-size (* 8 cpus)})))
                     ;; none
                     :else (constantly nil))]
    (-> (ring/ring-handler
          (ring/router
            [["/plaintext" (exchange/constantly plain-text-handler)]
             ["/json" json-handler]
             ["/db" db-handler]])
          (ring/create-default-handler)
          {:inject-match? false
           :inject-router? false})
        (server/create
          {:port 8080
           :host "0.0.0.0"
           :io-threads (* 2 cpus)
           :worker-threads (* 8 cpus)})
        (server/start))))
