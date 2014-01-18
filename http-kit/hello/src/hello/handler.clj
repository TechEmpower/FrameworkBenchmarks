(ns hello.handler
  (:gen-class)
  (:use compojure.core
        ring.middleware.json
        org.httpkit.server
        [clojure.tools.cli :only [cli]]
        ring.util.response)
  (:require [compojure.handler :as handler]
            [org.httpkit.dbcp :as db]
            [compojure.route :as route]))

;;; convert to int
(defn to-int [s] (cond
                  (string? s) (Integer/parseInt s)
                  (instance? Integer s) s
                  (instance? Long s) (.intValue ^Long s)
                  :else 0))

;; Query a random World record from the database
(defn get-world []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    ; Set a naming strategy to preserve column name case
    (clojure.java.jdbc/with-naming-strategy {:keyword identity}
      (db/query "select * from world where id = ?" id))))

;; Run the specified number of queries, return the results
(defn run-queries [queries]
   (flatten ; Make it a list of maps
    (take
     queries ; Number of queries to run
     (repeatedly get-world))))

(defn get-query-count [queries]
  "Parse provided string value of query count, clamping values to between 1 and 500."
  (let [q (try (Integer/parseInt queries)
               (catch Exception e 1))] ; default to 1 on parse failure
    (if (> q 500)
      500 ; clamp to 500 max
      (if (< q 1)
        1 ; clamp to 1 min
        q)))) ; otherwise use provided value

;; Define route handlers
(defroutes app-routes
  (GET "/http-kit/" [] "Hello, World!")
  (GET "/http-kit/json" [] (response {:message "Hello, World!"}))
  (GET "/http-kit/db" []
       (response (first (run-queries 1))))
  (GET "/http-kit/db/:queries" [queries]
       (response (run-queries (get-query-count queries))))
  (route/not-found "Not Found"))


(defn start-server [{:keys [port db-host]}]
  (db/use-database! (str "jdbc:mysql://" db-host "/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true")
                    "benchmarkdbuser"
                    "benchmarkdbpass")
  ;; Format responses as JSON
  (let [handler (wrap-json-response app-routes)
        cpu (.availableProcessors (Runtime/getRuntime))]
    ;; double worker threads should increase database access performance
    (run-server handler {:port port :thread (* 2 cpu)})
    (println (str "http-kit server listens at :" port))))

(defn -main [& args]
  (let [[options _ banner]
        (cli args
             ["-p" "--port" "Port to listen" :default 8080 :parse-fn to-int]
             ["--db-host" "MySQL database host" :default "localhost"]
             ["--[no-]help" "Print this help"])]
    (when (:help options) (println banner) (System/exit 0))
    (start-server options)))
