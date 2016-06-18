(ns hello.routes.home
  (:require [hello.layout :as layout]
            [hello.db.core :as db]
            [compojure.core :refer [defroutes GET]]
            [ring.util.http-response :as response]
            [clojure.java.io :as io]
            [ring.util.response :refer [content-type]]
            [cheshire.core :refer [generate-string]]))

(defn encode-json-response [rsp]
  (-> rsp
      generate-string
      response/ok
      (content-type "application/json")))

(defn json-serialization
  "Test 1: JSON serialization"
  []
  (-> {:message "Hello, World!"}
      generate-string
      response/ok
      (content-type "application/json")))

(defn single-query-test
  "Test 2: Single database query"
  []
  (-> 1
      db/run-queries
      first
      encode-json-response))

(defn multiple-query-test
  "Test 3: Multiple database query"
  [queries]
  (-> queries
      db/run-queries
      encode-json-response))

(defn fortunes
  "Test 4: Fortunes"
  []
  (layout/render "home.html" {:messages (db/get-fortunes)}))

(defn db-update
  "Test 5: Database updates"
  [queries]
  (-> queries
      db/update-and-persist
      encode-json-response))

(def plaintext
  "Test 6: Plaintext"
  (->
    (response/ok "Hello, World!")
    (content-type "text/plain")))

(defroutes io-routes
  (GET "/plaintext"        [] plaintext)
  (GET "/json"             [] (json-serialization)))

(defroutes default-routes
  (GET "/"                 []        "Hello, World!")
  (GET "/db"               []        (single-query-test))
  (GET "/queries/"         []        (multiple-query-test 1))
  (GET "/queries/:queries" [queries] (multiple-query-test queries))
  (GET "/fortunes"         []        (fortunes))
  (GET "/updates/"         []        (db-update 1))
  (GET "/updates/:queries" [queries] (db-update queries)))
