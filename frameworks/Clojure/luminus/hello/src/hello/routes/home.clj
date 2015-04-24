(ns hello.routes.home
  (:use compojure.core
        hello.models.db)
  (:require [hello.views.layout :as layout]
            [noir.response :as response]))


(def json-serialization
  "Test 1: JSON serialization"
  (response/json {:message "Hello, World!"}))


(def single-query-test
  "Test 2: Single database query"
  (response/json (first (run-queries 1))))


(defn multiple-query-test
  "Test 3: Multiple database query"
  [queries]
  (-> queries
      (get-query-count)
      (run-queries)
      (response/json)))


(def single-query-test-raw
  "Test 2: Single database query (raw)"
  (-> 1
      (run-queries-raw)
      (first)
      (response/json)))


(defn multiple-query-test-raw
  "Test 3: Multiple database query (raw)"
  [queries]
  (-> queries
      (get-query-count)
      (run-queries-raw)
      (response/json)))


(def fortunes
  "Test 4: Fortunes"
  (layout/render "home.html"
                 {:messages get-fortunes}))


(defn db-update
  "Test 5: Database updates"
  [queries]
  (-> queries
      (get-query-count)
      (update-and-persist)
      (response/json)))


(def plaintext
  "Test 6: Plaintext"
  {:status 200
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body "Hello, World!"})


(defroutes home-routes
  (GET "/"                [] "Hello, World!")
  (GET "/plaintext"       [] plaintext)
  (GET "/json"            [] json-serialization)
  (GET "/db"              [] single-query-test)
  (GET "/db/:queries"     [queries] (multiple-query-test queries))
  (GET "/dbraw"           [] single-query-test-raw)
  (GET "/dbraw/:queries"  [queries] (multiple-query-test-raw queries))
  (GET "/fortune"         [] fortunes)
  (GET "/update/:queries" [queries] (db-update queries)))
