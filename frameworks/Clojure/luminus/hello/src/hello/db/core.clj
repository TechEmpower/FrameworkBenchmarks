(ns hello.db.core
  (:require
    [clojure.java.jdbc :as jdbc]
    [conman.core :as conman]
    [environ.core :refer [env]]
    [mount.core :refer [defstate]])
  (:import [java.sql
            BatchUpdateException
            PreparedStatement]))

(def pool-spec
  {:jdbc-uri   "jdbc:mysql://localhost:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
   :datasource-classname "com.mysql.jdbc.jdbc2.optional.MysqlDataSource"
   :username   "benchmarkdbuser"
   :password   "benchmarkdbpass"
   :init-size  1
   :min-idle   1
   :max-idle   4
   :max-active 32})

(defn connect! []
  (let [conn (atom nil)]
    (conman/connect! conn pool-spec)
    conn))

(defn disconnect! [conn]
  (conman/disconnect! conn))

(defstate ^:dynamic *db*
          :start (connect!)
          :stop (disconnect! *db*))

(conman/bind-connection *db* "sql/queries.sql")

(defn to-date [sql-date]
  (-> sql-date (.getTime) (java.util.Date.)))

(extend-protocol jdbc/IResultSetReadColumn
  java.sql.Date
  (result-set-read-column [v _ _] (to-date v))

  java.sql.Timestamp
  (result-set-read-column [v _ _] (to-date v)))

(extend-type java.util.Date
  jdbc/ISQLParameter
  (set-parameter [v ^PreparedStatement stmt idx]
    (.setTimestamp stmt idx (java.sql.Timestamp. (.getTime v)))))

(defn get-world-random
  "Query a random World record between 1 and 10,000 from the database"
  []
  (get-world {:id (inc (rand-int 9999))}))

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
   (conj (get-all-fortunes {})
         {:id 0 :message "Additional fortune added at request time."})))

(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (for [world (-> queries run-queries)]
    (let [updated-world (assoc world :randomNumber (inc (rand-int 9999)))]
      (update-world<! updated-world)
      updated-world)))

