(ns hello.handler
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:use compojure.core
        ring.middleware.json
        ring.util.response
        korma.db
        korma.core
        hiccup.core
        hiccup.util)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.java.jdbc :as jdbc]
            [clojure.java.jdbc.sql :as sql]
            [net.cgrand.enlive-html :as html]))

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

; Query a random World record from the database
(defn get-world []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (select world
            (fields :id :randomNumber)
            (where {:id id }))))

; Run the specified number of queries, return the results
(defn run-queries [queries]
  (vec ; Return as a vector
   (flatten ; Make it a list of maps
    (take
     queries ; Number of queries to run
     (repeatedly get-world)))))

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

; Query a random World record from the database
(defn get-world-raw []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (jdbc/with-connection (db-raw)
      (jdbc/with-query-results rs [(str "select * from world where id = ?") id]
        (doall rs)))))

; Run the specified number of queries, return the results
(defn run-queries-raw [queries]
  (vec ; Return as a vector
   (flatten ; Make it a list of maps
    (take
     queries ; Number of queries to run
     (repeatedly get-world-raw)))))

(defn get-query-count [queries]
  "Parse provided string value of query count, clamping values to between 1 and 500."
  (let [q (try (Integer/parseInt queries)
               (catch Exception e 1))] ; default to 1 on parse failure
    (if (> q 500)
      500 ; clamp to 500 max
      (if (< q 1)
        1 ; clamp to 1 min
        q)))) ; otherwise use provided value


; Set up entity World and the database representation
(defentity fortune
  (pk :id)
  (table :fortune)
  (entity-fields :id :message)
  (database db))

(defn get-all-fortunes []
  "Query all Fortune records from the database."
    (select fortune
            (fields :id :message)))

(defn get-fortunes []
  "Fetch the full list of Fortunes from the database, sort them by the fortune
message text, and then return the results."
  (let [fortunes (conj (get-all-fortunes) {:id 0 :message "Additional fortune added at request time."} )]
    (sort-by #(:message %) fortunes)))

(defn fortunes-hiccup [fortunes]
  "Render the given fortunes to simple HTML using Hiccup."
  (html
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

(defn fortunes-enlive [fortunes]
  "Render the given fortunes to simple HTML using Enlive."
  "todo")

; Define route handlers
(defroutes app-routes
  (GET "/" [] "Hello, World!")
  (GET "/plaintext" []
       {:status 200
        :headers {"Content-Type" "text/plain; charset=utf-8"}
        :body "Hello, World!"})
  (GET "/json" [] (response {:message "Hello, World!"}))
  (GET "/db/:queries" [queries] (response (run-queries (get-query-count queries))))
  (GET "/dbraw/:queries" [queries] (response (run-queries-raw (get-query-count queries))))
  (GET "/fortune" [] (response (get-fortunes)))
  (GET "/fortune-hiccup" [] (fortunes-hiccup (get-fortunes)))
  (GET "/fortune-enlive" [] (fortunes-enlive (get-fortunes)))
  (route/not-found "Not Found"))

; Format responses as JSON
(def app
  (wrap-json-response app-routes))
