(ns hello.db.core
  (:require
    [cheshire.core :refer [generate-string parse-string]]
    [clojure.java.jdbc :as jdbc]
    [conman.core :as conman]
    [hello.config :refer [env]]
    [mount.core :refer [defstate]]))

(defstate ^:dynamic *db*
          :start (conman/connect!
                   {:init-size  10
                    :min-idle   1
                    :max-idle   10
                    :max-active 64
                    :maximum-pool-size 256
                    :jdbc-url   (:database-url env)})
          :stop (conman/disconnect! *db*))

(conman/bind-connection *db* "sql/queries.sql")

;; queries

(defn get-query-count
  "Parse provided string value of query count, clamping values to between 1 and 500."
  [^String queries]
  (let [n ^long (try (Integer/parseInt queries)
                     (catch Exception _ 1))] ; default to 1 on parse failure
    (cond
      (< ^long n 1) 1
      (> ^long n 500) 500
      :else n)))

(defn run-query []
  (get-world {:id (unchecked-inc ^long (rand-int 10000))}))

(defn run-queries
  "Run the specified number of queries, return the results"
  [^String queries]
  (let [query-count (get-query-count queries)]
    (repeatedly query-count #(get-world {:id (unchecked-inc ^long (rand-int 10000))}))))

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
  (let [w {:id id :randomnumber (unchecked-inc ^long (rand-int 9999))}]
    (try
        (update-world! w)
        (catch java.sql.BatchUpdateException e
          (throw (.getNextException e))))
    w))

(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [^String queries]
  (mapv set-random-number! (run-queries queries)))
