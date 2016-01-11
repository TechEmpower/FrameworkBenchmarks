(ns hello.handler
  (:gen-class)
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:use compojure.core
        ring.middleware.json
        org.httpkit.server
        [clojure.tools.cli :only [cli]]
        korma.db
        korma.core
        hiccup.core
        hiccup.util
        hiccup.page)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.util.response :as ring-resp]
            [clojure.java.jdbc :as jdbc]))

(defn sanitize-queries-param
  "Sanitizes the `queries` parameter. Clamps the value between 1 and 500.
  Invalid (string) values become 1."
  [queries]
  (let [n (try (Integer/parseInt queries)
               (catch Exception e 1))] ; default to 1 on parse failure
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))

;; MySQL database connection
(defdb db-mysql
  (mysql {
    :classname "com.mysql.jdbc.Driver"
    :subprotocol "mysql"
    :subname "//127.0.0.1:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
    :user "benchmarkdbuser"
    :password "benchmarkdbpass"
    ;;OPTIONAL KEYS
    :delimiters "" ;; remove delimiters
    :maximum-pool-size 256}))

;; MySQL database connection for java.jdbc "raw"
;; https://github.com/clojure/java.jdbc/blob/master/doc/clojure/java/jdbc/ConnectionPooling.md
(def db-spec-mysql-raw
  {:classname "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//127.0.0.1:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
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

(defn db-mysql-raw [] @pooled-db)

;; Set up entity World and the database representation
(defentity world
  (pk :id)
  (table :world)
  (entity-fields :id :randomNumber) ; Default fields for select
  (database db-mysql))

(defn get-random-world-korma
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (select world
            (where {:id id }))))

(defn run-queries
  "Run query repeatedly, return an array"
  [queries]
  (flatten ; Make it a list of maps
    (take queries ; Number of queries to run
          (repeatedly get-random-world-korma))))

(defn get-random-world-raw
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))]
    (jdbc/query (db-mysql-raw) [(str "select * from world where id = ?") id])))

(defn run-queries-raw
  "Run query repeatedly, return an array"
  [queries]
  (flatten ; Make it a list of maps
    (take queries
          (repeatedly get-random-world-raw))))

;; Set up entity Fortune and the database representation
(defentity fortune
  (pk :id)
  (table :fortune)
  (entity-fields :id :message)
  (database db-mysql))

(defn get-all-fortunes-korma
  "Query all Fortune records from the database using Korma."
  []
  (select fortune
          (fields :id :message)))

(defn get-all-fortunes-raw
  "Query all Fortune records from the database using JDBC."
  []
  (jdbc/query (db-mysql-raw) [(str "select * from fortune")]))

(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  [query-function]
  (sort-by #(:message %)
    (conj
      (query-function)
      { :id 0 :message "Additional fortune added at request time." })))

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

(defn update-and-persist-korma
  "Using Korma: Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
(let [results (run-queries queries)]
    (for [w results]
      (update-in w [:randomNumber (inc (rand-int 9999))]
        (update world
                (set-fields {:randomNumber (:randomNumber w)})
                (where {:id [:id w]}))))
    results))

(defn update-and-persist-raw
  "Using JDBC: Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
(let [results (run-queries queries)]
    (for [w results]
      (update-in w [:randomNumber (inc (rand-int 9999))]
        (jdbc/update! (db-mysql-raw) :world {:randomNumber (:randomNumber w)} ["id = ?" (:id w)])))
    results))

(def json-serialization
  "Test 1: JSON serialization"
  (ring-resp/response {:message "Hello, World!"}))

(def single-query-test
  "Test 2: Single database query"
  (ring-resp/response (first (run-queries 1))))

(defn multiple-queries-test
  "Test 3: Multiple database queries"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (run-queries)
      (ring-resp/response)))

(def single-query-test-raw
  "Test 2: Single database query (raw)"
  (-> 1
      (run-queries-raw)
      (first)
      (ring-resp/response)))

(defn multiple-queries-test-raw
  "Test 3: Multiple database queries (raw)"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (run-queries-raw)
      (ring-resp/response)))

(def fortune-test
  "Test 4: Fortunes"
  (->
    (get-fortunes get-all-fortunes-korma)
    (fortunes-hiccup)
    (ring-resp/response)
    (ring-resp/content-type "text/html")
    (ring-resp/charset "utf-8")))

(def fortune-test-raw
  "Test 4: Fortunes Raw"
  (->
    (get-fortunes get-all-fortunes-raw)
    (fortunes-hiccup)
    (ring-resp/response)
    (ring-resp/content-type "text/html")
    (ring-resp/charset "utf-8")))

(defn db-updates
  "Test 5: Database updates"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (update-and-persist-korma)
      (ring-resp/response)))

(defn db-updates-raw
  "Test 5: Database updates Raw"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (update-and-persist-raw)
      (ring-resp/response)))

(def plaintext
  "Test 6: Plaintext"
  (->
    (ring-resp/response "Hello, World!")
    (ring-resp/content-type "text/plain")))

;; Define route handlers
(defroutes app-routes
  (GET "/"                     [] "Hello, World!")
  (GET "/plaintext"            [] plaintext)
  (GET "/json"                 [] json-serialization)
  (GET "/db"                   [] single-query-test)
  (GET "/queries/:queries"     [queries] (multiple-queries-test queries))
  (GET "/queries/"             [] (multiple-queries-test queries)) ; When param is omitted
  (GET "/fortunes"             [] fortune-test)
  (GET "/updates/:queries"     [queries] (db-updates queries))
  (GET "/updates/"             [] (db-updates queries)) ; When param is omitted
  (GET "/raw/db"               [] single-query-test-raw)
  (GET "/raw/queries/:queries" [queries] (multiple-queries-test-raw queries))
  (GET "/raw/queries/"         [] (multiple-queries-test-raw queries)) ; When param is omitted
  (GET "/raw/fortunes"         [] fortune-test-raw)
  (GET "/raw/updates/:queries" [queries] (db-updates-raw queries))
  (GET "/raw/updates/"         [] (db-updates-raw queries)) ; When param is omitted
  (route/not-found "Not Found"))

(defn parse-port [s] 
  "Convert stringy port number int. Defaults to 8080."
  (cond
    (string? s) (Integer/parseInt s)
    (instance? Integer s) s
    (instance? Long s) (.intValue ^Long s)
    :else 8080))

(defn start-server [{:keys [port]}]
  ;; Format responses as JSON
  (let [handler (wrap-json-response app-routes)
        cpu (.availableProcessors (Runtime/getRuntime))]
    ;; double worker threads should increase database access performance
    (run-server handler {:port port
                         :thread (* 2 cpu)})
    (println (str "http-kit server listens at :" port))))


(defn -main [& args]
  (let [[options _ banner]
        (cli args
             ["-p" "--port" "Port to listen" :default 8080 :parse-fn parse-port]
             ["--[no-]help" "Print this help"])]
    (when (:help options)
          (println banner)
          (System/exit 0))
    (start-server options)))
