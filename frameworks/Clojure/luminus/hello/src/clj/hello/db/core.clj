(ns hello.db.core
  (:require
    [cheshire.core :refer [generate-string parse-string]]
    [clojure.java.jdbc :as jdbc]
    [conman.core :as conman]
    [hello.config :refer [env]]
    [mount.core :refer [defstate]]
    [clojure.set :refer [rename-keys]])
  (:import org.postgresql.util.PGobject
           org.postgresql.jdbc4.Jdbc4Array
           clojure.lang.IPersistentMap
           clojure.lang.IPersistentVector
           [java.sql
            BatchUpdateException
            Date
            Timestamp
            PreparedStatement]))

(defstate ^:dynamic *db*
          :start (conman/connect!
                   {:adapter    :postgresql
                    :init-size  10
                    :min-idle   1
                    :max-idle   10
                    :max-active 64
                    :jdbc-url   (:database-url env)})
          :stop (conman/disconnect! *db*))

(conman/bind-connection *db* "sql/queries.sql")

;; queries

(defn get-query-count [queries]
  "Parse provided string value of query count, clamping values to between 1 and 500."
  (let [n (try (Integer/parseInt queries)
               (catch Exception _ 1))] ; default to 1 on parse failure
    (cond
      (< n 1)   1
      (> n 500) 500
      :else     n)))

(defn get-random-world []
  (-> (get-world {:id (inc (rand-int 10000))})
      (rename-keys {:randomnumber :randomNumber})))

(defn run-queries
  "Run the specified number of queries, return the results"
  [queries]
  (repeatedly (get-query-count queries) get-random-world))

(defn get-fortunes []
   "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  (sort-by
   :message
   (conj (get-all-fortunes)
         {:id 0 :message "Additional fortune added at request time."})))

(defn set-random-number!
  "set a new randomNumber, persist, and return the record"
  [{:keys [id]}]
  (let [w {:id id :randomNumber (inc (rand-int 9999))}]
    (try
        (update-world! w)
        (catch java.sql.BatchUpdateException e
          (throw (.getNextException e))))
    w))

(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (doall (map set-random-number! (run-queries queries))))
