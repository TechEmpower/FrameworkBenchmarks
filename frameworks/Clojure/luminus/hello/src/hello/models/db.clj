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


(defn get-world
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (select world
            (fields :id :randomNumber)
            (where {:id id }))))


(defn get-world-raw
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (jdbc/with-connection (db-raw)
      ; Set a naming strategy to preserve column name case
      (jdbc/with-naming-strategy {:keyword identity}
        (jdbc/with-query-results rs [(str "select * from world where id = ?") id]
          (doall rs))))))


(defn run-queries
  "Run the specified number of queries, return the results"
  [queries]
  (flatten ; Make it a list of maps
    (take queries
          (repeatedly get-world))))


(defn run-queries-raw [queries]
  "Run the specified number of queries, return the results"
  (flatten ; Make it a list of maps
    (take queries
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


(def get-all-fortunes
  "Query all Fortune records from the database."
  (select fortune
          (fields :id :message)))


(def get-fortunes
  "Fetch the full list of Fortunes from the database. Insert an additional fortune at runtime.
  Then sort all by fortune message text. Return the results."
  (let [fortunes (conj get-all-fortunes
                       {:id 0 :message "Additional fortune added at request time."})]
    (sort-by :message fortunes)))


(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (let [results (run-queries queries)]
    (for [w results]
      (update-in w [:randomNumber (inc (rand-int 9999))]
        (update world
                (set-fields {:randomNumber (:randomNumber w)})
                (where {:id [:id w]}))))
    results))
