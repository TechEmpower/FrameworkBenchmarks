(ns hello.handler
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [compojure.core :refer [GET defroutes]]
            [compojure.route :as route]
            [hiccup.page :refer [html5]]
            [hiccup.util :refer [escape-html]]
            [hikari-cp.core :refer :all]
            [ring.middleware.json :as json-middleware]
            [ring.util.response :as ring-resp]))

(defn sanitize-queries-param
  "Sanitizes the `queries` parameter. Clamps the value between 1 and 500.
  Invalid (string) values become 1."
  [queries]
  (let [n (try (Integer/parseInt queries)
               (catch Exception _ 1))]
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))

(defn make-hikari-data-source
  []
  (make-datasource {
                    :idle-timeout      15000
                    :max-lifetime      60000
                    :minimum-idle      0
                    :maximum-pool-size 1024
                    :pool-name         "db-pool"
                    :driver-class-name "com.mysql.cj.jdbc.Driver"
                    :jdbc-url          "jdbc:mysql://tfb-database:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts=true&cacheRSMetadata=true&useSSL=false"
                    :username          "benchmarkdbuser"
                    :password          "benchmarkdbpass"
                    :register-mbeans   false}))

(def memoize-hikari-data-source (memoize make-hikari-data-source))

(defn db-mysql [] (memoize-hikari-data-source))


(defn get-random-world
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))]
    (jdbc/execute! (db-mysql)
                   ["select * from world where id = ?" id]
                   {:builder-fn rs/as-unqualified-lower-maps})))

(defn run-queries
  "Run query repeatedly, return an array"
  [queries]
  (flatten
    (take queries
          (repeatedly get-random-world))))


(defn get-all-fortunes
  "Query all Fortune records from the database using next.jdbc."
  []
  (jdbc/execute! (db-mysql)
                 ["select * from fortune"]
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  [query-function]
  (sort-by #(:message %)
           (conj
             (query-function)
             {:id 0 :message "Additional fortune added at request time."})))

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
  "Using next.jdbc: Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (let [world (map #(assoc % :randomnumber (inc (rand-int 9999))) (run-queries queries))]
    (doseq [{:keys [id randomnumber]} world]
      (jdbc/execute-one!
        (db-mysql)
        ["UPDATE world SET randomnumber = ? WHERE id = ?" randomnumber id]))
    world))

(defn json-serialization
  "Test 1: JSON serialization"
  []
  (ring-resp/response {:message "Hello, World!"}))

(defn single-query-test
  "Test 2: Single database query"
  []
  (-> 1
      (run-queries)
      (first)
      (ring-resp/response)))

(defn multiple-queries-test
  "Test 3: Multiple database queries"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (run-queries)
      (ring-resp/response)))

(defn fortune-test
  "Test 4: Fortunes"
  []
  (->
    (get-fortunes get-all-fortunes)
    (fortunes-hiccup)
    (ring-resp/response)
    (ring-resp/content-type "text/html")
    (ring-resp/charset "utf-8")))

(defn db-updates
  "Test 5: Database updates"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (update-and-persist)
      (ring-resp/response)))

(def plaintext
  "Test 6: Plaintext"
  (->
    (ring-resp/response "Hello, World!")
    (ring-resp/content-type "text/plain")))

;; Define route handlers
(defroutes app-routes
           (GET "/" [] "Hello, World!")
           (GET "/plaintext" [] plaintext)
           (GET "/json" [] (json-serialization))
           (GET "/db" [] (single-query-test))
           (GET "/queries/:queries" [queries] (multiple-queries-test queries))
           (GET "/queries/" [] (multiple-queries-test 1))
           (GET "/fortunes" [] (fortune-test))
           (GET "/updates/:queries" [queries] (db-updates queries))
           (GET "/updates/" [] (db-updates 1))
           (route/not-found "Not Found"))

(def app
  "Format responses as JSON"
  (-> app-routes
      (json-middleware/wrap-json-response)))