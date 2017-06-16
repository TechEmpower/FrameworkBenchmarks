(ns hello.db
  (:require
    [cljs.nodejs :as node]
    [mount.core :refer [defstate]]))

(def sequelize (node/require "sequelize"))

(defstate db
  :start (sequelize.
           "hello_world"
           "benchmarkdbuser"
           "benchmarkdbpass"
           #js {:host    "localhost"
                :dialect "postgres"
                :logging false
                :pool #js {:max 64
                           :min 1
                           :idle 1000}}))

(defstate worlds
  :start (.define @db
                  "World"
                  #js{:id           #js {:type       "Sequelize.INTEGER"
                                         :primaryKey true}
                      :randomnumber {:type "Sequelize.INTEGER"}}
                  #js {:timestamps      false
                       :freezeTableName true
                       :tableName       "World"}))

(defstate fortunes
  :start (.define @db
                  "Fortune"
                  #js {:id      #js {:type       "Sequelize.INTEGER"
                                     :primaryKey true}
                       :message {:type "Sequelize.STRING"}}
                  #js {:timestamps      false
                       :freezeTableName true
                       :tableName       "Fortune"}))

(defn all-fortunes [handler error-handler]
  (-> @fortunes
      (.findAll #js {:raw true})
      (.then #(handler (js->clj % :keywordize-keys true)))
      (.catch error-handler)))

(defn world-promise [id]
  (.findOne @worlds (clj->js {:where {:id id} :raw true})))

(defn world [id handler error-handler]
  (-> (world-promise id)
      (.then handler)
      (.catch error-handler)))

(defn update-world! [world handler error-handler]
  (-> @worlds
      (.update (clj->js {:randomnumber (:randomnumber world)})
               (clj->js {:where {:id (:id world)} :raw true}))
      (.then #(when handler (handler (js->clj world))))
      (.catch error-handler)))

(defn get-query-count
  "Parse provided string value of query count, clamping values to between 1 and 500."
  [queries]
  (let [n (js/parseInt queries)]                            ; default to 1 on parse failure
    (cond
      (js/isNaN n) 1
      (< n 1) 1
      (> n 500) 500
      :else n)))

(defn run-query [handler error-handler]
  (world (unchecked-inc (rand-int 10000)) handler error-handler))

(defn run-queries
  "Run the specified number of queries, return the results"
  [queries handler error-handler]
  (-> (get-query-count queries)
      (repeatedly #(world-promise (unchecked-inc (rand-int 10000))))
      (js/Promise.all)
      (.then handler)
      (.catch error-handler)))

(defn get-fortunes [handler error-handler]
  "Fetch the full list of Fortunes from the database, sort them by the fortune
 message text, and then return the results."
  (all-fortunes
    (fn [result]
      (handler
        (sort-by
          :message
          (conj result {:id 0 :message "Additional fortune added at request time."}))))
    error-handler))

(defn set-random-number!
  "set a new randomNumber, persist, and return the record"
  [error-handler world]
  (let [w (assoc world :randomnumber (unchecked-inc (rand-int 9999)))]
    (update-world! w nil error-handler)
    w))

(defn update-and-persist
  "Changes the :randomnumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries handler error-handler]
  (run-queries
    queries
    (fn [results]
      (handler (clj->js (mapv (partial set-random-number! error-handler) (js->clj results)))))
    error-handler))

