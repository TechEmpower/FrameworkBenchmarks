(ns pedestal.service
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:use korma.db
        korma.core
        hiccup.core
        hiccup.util
        hiccup.page)
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]
            [ring.util.response :as ring-resp]
            [clojure.data.json :as json]
            [clojure.java.jdbc :as jdbc]))

(defn sanitize-queries-param
  "Sanitizes the `queries` parameter. Clamps the value between 1 and 500.
  Invalid (string) values become 1."
  [request]
  (let [queries (-> request
                    :params
                    :queries)
        n (try (Integer/parseInt queries)
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
    :subname "//127.0.0.1:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts=true&cacheRSMetadata=true&useSSL=false"
    :user "benchmarkdbuser"
    :password "benchmarkdbpass"
    ;;OPTIONAL KEYS
    :delimiters "" ;; remove delimiters
    :maximum-pool-size 256}))


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


(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  []
  (sort-by #(:message %)
    (conj
      (get-all-fortunes-korma)
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

(defn update-and-persist
  "Using Korma: Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (let [results (map #(assoc % :randomNumber (inc (rand-int 9999))) (run-queries queries))]
    (doseq [{:keys [id randomNumber]} results]
      (update world
              (set-fields {:randomNumber randomNumber})
              (where {:id id})))
    results))

(defn json-serialization
  "Test 1: JSON serialization"
  [request]
  (http/json-response {:message "Hello, World!"}))

(defn single-query-test
  "Test 2: Single database query"
  [request]
  (http/json-response (first (run-queries 1))))

(defn multiple-queries-test
  "Test 3: Multiple database queries"
  [request]
  (-> request
      (sanitize-queries-param)
      (run-queries)
      (http/json-response)))

(defn fortune-test
  "Test 4: Fortunes"
  [request]
  (->
    (get-fortunes)
    (fortunes-hiccup)
    (ring-resp/response)
    (ring-resp/content-type "text/html")
    (ring-resp/charset "utf-8")))

(defn db-updates
  "Test 5: Database updates"
  [request]
  (-> request
      (sanitize-queries-param)
      (update-and-persist)
      (http/json-response)))


(defn plaintext
  "Test 6: Plaintext"
  [request]
  (ring-resp/response "Hello, World!"))

;; Define route handlers
(defroutes routes
  [[
  [  "/plaintext" {:get plaintext}]
  [  "/json"      {:get json-serialization}]
  [  "/db"        {:get single-query-test}]
  [  "/queries"   {:get multiple-queries-test}]
  [  "/fortunes"  {:get fortune-test}]
  [  "/updates"   {:get db-updates}]]])


(def service
  "How the server will look, not the code to start it up"
  { :env :prod
    ::http/routes routes
    ::http/resource-path "/public"
    ::http/type :jetty
    ::http/port 8080})
