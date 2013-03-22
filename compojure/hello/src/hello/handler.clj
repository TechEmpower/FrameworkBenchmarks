(ns hello.handler
  (:use compojure.core
        ring.middleware.json
        ring.util.response
        korma.db
        korma.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

; Database connection
(defdb db (mysql {:db "hello_world"
                       :user "benchmarkdbuser"
                       :password "benchmarkdbpass"
                       ;;OPTIONAL KEYS
                       :host "localhost"
                       :port "3306"
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

; Define route handlers
(defroutes app-routes
  (GET "/" [] "Hello, World!")
  (GET "/json" [] (response {:message "Hello, World!"}))
  (GET "/db/:queries" [queries] (response (run-queries (Integer/parseInt queries))))
  (route/not-found "Not Found"))

; Format responses as JSON
(def app
  (wrap-json-response app-routes))
