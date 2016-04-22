(ns hello.handler
  (:use compojure.core
        ring.middleware.content-type
        ring.middleware.json
        korma.db
        korma.core
        hiccup.core
        hiccup.util
        hiccup.page)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.util.response :as ring-resp]
            [clojure.java.jdbc :as jdbc]
            [hikari-cp.core :refer :all]))

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

;; MySQL database connection for java.jdbc "raw" using HikariCP
(def datasource-options-hikaricp {:auto-commit        true
                                  :read-only          false
                                  :connection-timeout 30000
                                  :validation-timeout 5000
                                  :idle-timeout       600000
                                  :max-lifetime       1800000
                                  :minimum-idle       10
                                  :maximum-pool-size  256
                                  :pool-name          "db-pool"
                                  :adapter            "mysql"
                                  :username           "benchmarkdbuser"
                                  :password           "benchmarkdbpass"
                                  :database-name      "hello_world"
                                  :server-name        "127.0.0.1"
                                  :port-number        3306
                                  :register-mbeans    false})

;; Create HikariCP-pooled "raw" jdbc data source
(def db-spec-mysql-raw-hikaricp
  (make-datasource datasource-options-hikaricp))

;; Get a HikariCP-pooled "raw" jdbc connection
(defn db-mysql-raw [] {:datasource db-spec-mysql-raw-hikaricp})

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
  (let [results (map #(assoc % :randomNumber (inc (rand-int 9999))) (run-queries queries))]
    (doseq [{:keys [id randomNumber]} results]
      (update world
              (set-fields {:randomNumber randomNumber})
              (where {:id id})))
    results))

(defn update-and-persist-raw
  "Using JDBC: Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (let [world (map #(assoc % :randomnumber (inc (rand-int 9999))) (run-queries-raw queries))]
    (doseq [{:keys [id randomnumber]} world]
      (jdbc/update!
       (db-mysql-raw)
       :world {:randomnumber randomnumber}
       ["id = ?" id]))
    world))

(defn json-serialization
  "Test 1: JSON serialization"
  []
  (ring-resp/response {:message "Hello, World!"}))

(defn single-query-test
  "Test 2: Single database query"
  []
  (ring-resp/response (first (run-queries 1))))

(defn multiple-queries-test
  "Test 3: Multiple database queries"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (run-queries)
      (ring-resp/response)))

(defn single-query-test-raw
  "Test 2: Single database query (raw)"
  []
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

(defn fortune-test
  "Test 4: Fortunes"
  []
  (->
    (get-fortunes get-all-fortunes-korma)
    (fortunes-hiccup)
    (ring-resp/response)
    (ring-resp/content-type "text/html")
    (ring-resp/charset "utf-8")))

(defn fortune-test-raw
  "Test 4: Fortunes Raw"
  []
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
  (GET "/json"                 [] (json-serialization))
  (GET "/db"                   [] (single-query-test))
  (GET "/queries/:queries"     [queries] (multiple-queries-test queries))
  (GET "/queries/"             [] (multiple-queries-test 1)) ; When param is omitted
  (GET "/fortunes"             [] (fortune-test))
  (GET "/updates/:queries"     [queries] (db-updates queries))
  (GET "/updates/"             [] (db-updates 1)) ; When param is omitted
  (GET "/raw/db"               [] (single-query-test-raw))
  (GET "/raw/queries/:queries" [queries] (multiple-queries-test-raw queries))
  (GET "/raw/queries/"         [] (multiple-queries-test-raw 1)) ; When param is omitted
  (GET "/raw/fortunes"         [] (fortune-test-raw))
  (GET "/raw/updates/:queries" [queries] (db-updates-raw queries))
  (GET "/raw/updates/"         [] (db-updates-raw 1)) ; When param is omitted
  (route/not-found "Not Found"))

(def app
  "Format responses as JSON"
  (-> app-routes
      (wrap-json-response)))
