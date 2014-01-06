(ns hello.models.db
  (:use korma.core
         hello.models.schema)
  (:require [clojure.java.jdbc :as jdbc]))



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

; Query a random World record from the database
(defn get-world-raw []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (jdbc/with-connection (db-raw)
      (jdbc/with-query-results rs [(str "select * from world where id = ?") id]
        (doall rs)))))

; Run the specified number of queries, return the results
(defn run-queries [queries]
   (flatten ; Make it a list of maps
    (take
     queries ; Number of queries to run
     (repeatedly get-world))))

; Run the specified number of queries, return the results
(defn run-queries-raw [queries]
   (flatten ; Make it a list of maps
    (take
     queries ; Number of queries to run
     (repeatedly get-world-raw))))

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
    (sort-by :message fortunes)))
