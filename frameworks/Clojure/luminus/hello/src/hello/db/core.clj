(ns hello.db.core
  (:require
    [cheshire.core :refer [generate-string parse-string]]
    [clojure.java.jdbc :as jdbc]
    [conman.core :as conman]
    [environ.core :refer [env]]
    [mount.core :refer [defstate]])
  (:import org.postgresql.util.PGobject
           org.postgresql.jdbc.PgArray
           clojure.lang.IPersistentMap
           clojure.lang.IPersistentVector
           [java.sql
            BatchUpdateException
            Date
            Timestamp
            PreparedStatement]))

(def pool-spec
  {:username   "benchmarkdbuser"
   :password   "benchmarkdbpass"
   :jdbc-url   "jdbc:postgresql://127.0.0.1:5432/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
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
  Date
  (result-set-read-column [v _ _] (to-date v))

  Timestamp
  (result-set-read-column [v _ _] (to-date v))

  PgArray
  (result-set-read-column [v _ _] (vec (.getArray v)))

  PGobject
  (result-set-read-column [pgobj _metadata _index]
    (let [type  (.getType pgobj)
          value (.getValue pgobj)]
      (case type
        "json" (parse-string value true)
        "jsonb" (parse-string value true)
        "citext" (str value)
        value))))

(extend-type java.util.Date
  jdbc/ISQLParameter
  (set-parameter [v ^PreparedStatement stmt idx]
    (.setTimestamp stmt idx (Timestamp. (.getTime v)))))

(defn to-pg-json [value]
  (doto (PGobject.)
    (.setType "jsonb")
    (.setValue (generate-string value))))

(extend-protocol jdbc/ISQLValue
  IPersistentMap
  (sql-value [value] (to-pg-json value))
  IPersistentVector
  (sql-value [value] (to-pg-json value)))

;; queries

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

