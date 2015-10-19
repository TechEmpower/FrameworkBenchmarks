(ns hello.handler
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:use compojure.core
        ring.middleware.json
        ring.util.response
        korma.db
        korma.core
        hiccup.core
        hiccup.util
        hiccup.page)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.java.jdbc :as jdbc]
            [clojure.java.jdbc.sql :as sql]))


; Database connection
(defdb db (mysql {:subname "//localhost:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
                  :user "benchmarkdbuser"
                  :password "benchmarkdbpass"
                  ;;OPTIONAL KEYS
                  :delimiters "" ;; remove delimiters
                  :maximum-pool-size 256
                  }))


; Set up entity World and the database representation
(defentity world
  (pk :id)
  (table :world)
  (entity-fields :id :randomNumber)
  (database db))


(defn get-world
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (select world
            (fields :id :randomNumber)
            (where {:id id }))))


(defn run-queries
  "Run the specified number of queries, return the results"
  [queries]
  (flatten ; Make it a list of maps
    (take queries ; Number of queries to run
          (repeatedly get-world))))


; Database connection for java.jdbc "raw"
; https://github.com/clojure/java.jdbc/blob/master/doc/clojure/java/jdbc/ConnectionPooling.md
(def db-spec-mysql-raw
  {:classname "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//localhost:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
   :user "benchmarkdbuser"
   :password "benchmarkdbpass"})


(defn pool
  [spec]
  (let [cpds (doto (ComboPooledDataSource.)
               (.setDriverClass (:classname spec))
               (.setJdbcUrl (str "jdbc:" (:subprotocol spec) ":" (:subname spec)))
               (.setUser (:user spec))
               (.setPassword (:password spec))
               ;; expire excess connections after 30 minutes of inactivity:
               (.setMaxIdleTimeExcessConnections (* 30 60))
               ;; expire connections after 3 hours of inactivity:
               (.setMaxIdleTime (* 3 60 60)))]
    {:datasource cpds}))


(def pooled-db (delay (pool db-spec-mysql-raw)))


(defn db-raw [] @pooled-db)


(defn get-world-raw
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))]
    (jdbc/with-connection (db-raw)
      ; Set a naming strategy to preserve column name case
      (jdbc/with-naming-strategy {:keyword identity}
        (jdbc/with-query-results rs [(str "select * from world where id = ?") id]
          (doall rs))))))


(defn run-queries-raw
  "Run the specified number of queries, return the results"
  [queries]
  (flatten ; Make it a list of maps
    (take queries
          (repeatedly get-world-raw))))


(defn get-query-count
  "Parse provided string value of query count, clamping values to between 1 and 500."
  [queries]
  (let [n (try (Integer/parseInt queries)
               (catch Exception e 1))] ; default to 1 on parse failure
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))


; Set up entity World and the database representation
(defentity fortune
  (pk :id)
  (table :fortune)
  (entity-fields :id :message)
  (database db))


(defn get-all-fortunes
  "Query all Fortune records from the database."
  []
  (select fortune
          (fields :id :message)))


(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  []
  (let [fortunes (conj (get-all-fortunes)
                       {:id 0
                        :message "Additional fortune added at request time."})]
    (sort-by #(:message %) fortunes)))


(defn fortunes-hiccup
  "Render the given fortunes to simple HTML using Hiccup."
  [fortunes]
  (html5
   [:head
    [:title "Fortunes"]]
   [:body
    [:table
     [:tr
      [:th "id"]
      [:th "message"]]
     (for [x fortunes]
       [:tr
        [:td (:id x)]
        [:td (escape-html (:message x))]])
     ]]))


(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (let [results (run-queries queries)]
    (for [w results]
      (update-in w [:randomNumber (inc (rand-int 9999))]
        (update world
                (set-fields {:randomNumber (:randomNumber w)})
                (where {:id [:id w]}))))
    results))


(def json-serialization
  "Test 1: JSON serialization"
  (response {:message "Hello, World!"}))


(def single-query-test
  "Test 2: Single database query"
  (-> 1
      (run-queries)
      (first)
      (response)))


(defn multiple-query-test
  "Test 3: Multiple database queries"
  [queries]
  (-> queries
      (get-query-count)
      (run-queries)
      (response)))


(def single-query-test-raw
  "Test 2: Single database query (raw)"
  (-> 1
      (run-queries-raw)
      (first)
      (response)))


(defn multiple-query-test-raw
  "Test 3: Multiple database queries (raw)"
  [queries]
  (-> queries
      (get-query-count)
      (run-queries-raw)
      (response)))


(def fortune-test
  "Test 4: Fortunes"
  (response (fortunes-hiccup (get-fortunes))))


(defn db-updates
  "Test 5: Database updates"
  [queries]
  (-> queries
      (get-query-count)
      (update-and-persist)
      (response)))

(def plaintext
  "Test 6: Plaintext"
  {:status 200
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body "Hello, World!"})


(defroutes app-routes
  (GET "/"                 [] "Hello, World!")
  (GET "/json"             [] json-serialization)
  (GET "/db"               [] single-query-test)
  (GET "/db/:queries"      [queries] (multiple-query-test queries))
  (GET "/dbraw"            [] single-query-test-raw)
  (GET "/dbraw/:queries"   [queries] (multiple-query-test-raw queries))
  (GET "/fortunes"         [] (response (get-fortunes))) ; Raw json of fortunes
  (GET "/fortune-hiccup"   [] fortune-test)
  (GET "/updates/:queries" [queries] (db-updates queries))
  (GET "/plaintext"        [] plaintext)
  (route/not-found "Not Found"))


(def app
  "Format responses as JSON"
  (wrap-json-response app-routes))
