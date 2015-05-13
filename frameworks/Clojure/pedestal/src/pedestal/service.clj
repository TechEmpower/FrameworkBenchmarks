(ns pedestal.service
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:use korma.db
        korma.core
        hiccup.core
        hiccup.util
        hiccup.page)
  (:require [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]
            [ring.util.response :as ring-resp]
            [clojure.data.json :as json]
            [clojure.java.jdbc :as jdbc]))


(defn json-serialization
  "Test 1: JSON serialization"
  [request]
  (bootstrap/json-response {:message "Hello, World!"}))


;; MySQL connection
(defdb mysql-db
  (mysql {
    :classname "com.mysql.jdbc.Driver"
    :subprotocol "mysql"
    :subname "//localhost:3306/hello_world"
    :user "benchmarkdbuser"
    :password "benchmarkdbpass"
    ;;OPTIONAL KEYS
    :delimiters "" ;; remove delimiters
    :maximum-pool-size 256}))


;; Set up entity World and the database representation
(defentity world
  (pk :id)
  (table :world)
  (entity-fields :id :randomNumber) ;; Default fields for select
  (database mysql-db))


(defn random-world
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (select world
            (where {:id id }))))


(defn run-queries
  "Run query repeatedly -- Always returns an array"
  [queries]
  (flatten (take queries (repeatedly random-world))))


(defn single-query-test
  "Test 2: Single database query"
  [request]
  (bootstrap/json-response (first (run-queries 1))))


(defn sanitizeQueriesParam
  "Sanitizes the `queries` parameter. Caps the value between 1 and 500.
  Invalid (stringy) values become 1"
  [request]
  (let [queries (-> request
                    :params
                    :queries)]
    (if-let [n (if (= (re-find #"\A-?\d+" queries) nil)
                   1
                   (Integer/parseInt queries))]
      (cond
        (< n 1) 1
        (> n 500) 500
        :else n))))


(defn multiple-query-test
  "Test 3: Multiple database queries"
  [request]
  (-> request
      (sanitizeQueriesParam)
      (run-queries)
      (bootstrap/json-response)))


; Set up entity Fortune and the database representation
(defentity fortune
  (pk :id)
  (table :fortune)
  (entity-fields :id :message)
  (database mysql-db))


(defn get-all-fortunes
  "Query all Fortune records from the database."
  []
  (select fortune
          (fields :id :message)))


(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  []
  (sort-by #(:message %)
    (conj
      (get-all-fortunes)
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


(defn fortune-test
  "Test 4: Fortunes"
  [request]
  (->
    (get-fortunes)
    (fortunes-hiccup)
    (ring-resp/response)
    (ring-resp/content-type "text/html")
    (ring-resp/charset "utf-8")))         ;; Apply charset after content type


(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [request]
  (let [results (-> request
                    (sanitizeQueriesParam)
                    (run-queries))]
    (for [w results]
      (update-in w [:randomNumber (inc (rand-int 9999))]
        (update world
                (set-fields {:randomNumber (:randomNumber w)})
                (where {:id [:id w]}))))
    results))


(defn db-updates
  "Test 5: Database updates"
  [request]
  (-> request
      (update-and-persist)
      (bootstrap/json-response)))


(defn plaintext
  "Test 6: Plaintext"
  [request]
  (ring-resp/response "Hello, World!"))


(defroutes routes
  [[
  [  "/json"      {:get json-serialization}]
  [  "/db"        {:get single-query-test}]
  [  "/queries"   {:get multiple-query-test}]
  [  "/fortunes"  {:get fortune-test}]
  [  "/updates"   {:get db-updates}]
  [  "/plaintext" {:get plaintext}]]])


(def service
  "How the server will look, not the code to start it up"
  { :env :prod
    ::bootstrap/routes routes
    ::bootstrap/resource-path "/public"
    ::bootstrap/type :jetty
    ::bootstrap/port 8080})
