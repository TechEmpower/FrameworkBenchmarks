(ns pedestal.service
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:use korma.db
        korma.core)
  (:require [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]
            [ring.util.response :as ring-resp]
            [clojure.data.json :as json]
            [clojure.java.jdbc :as jdbc]))

(defn json-serialization
  [request]
  (bootstrap/json-response {:message "Hello, World!"}))

(defn plaintext
  [request]
  (ring-resp/response "Hello, World!"))

;; Database Tests
;; Adopted from compojure/hello/src/hello/handler.clj
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

;; Query a random World record from the database
(defn random-world []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (select world
      (where {:id id }))))

;; Run query repeatedly -- Always returns an array
(defn run-queries
  [queries]
  (flatten (take queries (repeatedly random-world))))

(defn single-query-test
  [request] 
  (bootstrap/json-response (first (run-queries 1))))

;; http://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number
(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn multiple-query-test
  [request]
  (let [n (parse-int
    (let [queries (-> request :params :queries)]
      (if (= (re-find #"\A-?\d+" queries) nil) ; Guarantee queries to be numeric-looking
        "1"
        queries)))] ; queries now safely parsed into n as int
    (bootstrap/json-response
      (run-queries
        (cond
          (< n 1) 1
          (> n 500) 500
          :else n)))))


;; All of the available routes
(defroutes routes
  [[
  [  "/json"      {:get json-serialization}]
  [  "/plaintext" {:get plaintext}]
  [  "/db"        {:get single-query-test}]
  [  "/queries"   {:get multiple-query-test}]]])

;; How the server will look, not the code to start it up
(def service {:env :prod
              ::bootstrap/routes routes
              ::bootstrap/resource-path "/public"
              ::bootstrap/type :jetty
              ::bootstrap/port 8080})
