(ns hello.db.core
  (:require
    [yesql.core :refer [defqueries]])
  (:import com.mchange.v2.c3p0.ComboPooledDataSource))

(def db-spec
  {:classname "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//localhost:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
   :user "benchmarkdbuser"
   :password "benchmarkdbpass"})

(defn pool
  [spec]
  {:datasource
   (doto (ComboPooledDataSource.)
     (.setDriverClass (:classname spec))
     (.setJdbcUrl (str "jdbc:" (:subprotocol spec) ":" (:subname spec)))
     (.setUser (:user spec))
     (.setPassword (:password spec))
     ;; expire excess connections after 30 minutes of inactivity:
     (.setMaxIdleTimeExcessConnections (* 30 60))
     ;; expire connections after 3 hours of inactivity:
     (.setMaxIdleTime (* 3 60 60)))})

(defonce db (atom nil))

(defn connect! []
  (reset! db (pool db-spec)))

(defqueries "sql/queries.sql")

(defn get-world-random
  "Query a random World record between 1 and 10,000 from the database"
  []
  (get-world {:id (inc (rand-int 9999))} {:connection @db}))

(defn get-query-count [queries]
  "Parse provided string value of query count, clamping values to between 1 and 500."
  (let [n (try (Integer/parseInt queries)
               (catch Exception e 1))] ; default to 1 on parse failure
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))

(defn run-queries
  "Run the specified number of queries, return the results"
  [queries]
  (flatten (repeatedly (get-query-count queries) get-world-random)))

(defn get-fortunes []
   "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  (sort-by
   :message
   (conj (get-all-fortunes {} {:connection @db})
         {:id 0 :message "Additional fortune added at request time."})))

(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (for [world (-> queries run-queries)]
    (let [updated-world (assoc world :randomNumber (inc (rand-int 9999)))]
      (update-world<! updated-world {:connection @db})
      updated-world)))


