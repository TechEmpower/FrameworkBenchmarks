(ns hello.db
  (:require
    [cljs.nodejs :as node]
    [mount.core :refer [defstate]]))

(def pg ((node/require "pg-promise")))

(def sequelize (node/require "sequelize"))

(defstate db
  :start (sequelize.
           "hello_world"
           "benchmarkdbuser"
           "benchmarkdbpass"
           #js {:host    "localhost",
                :dialect "postgres",
                :logging false}))

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
      (.then #(handler (js->clj %) #_(js/JSON.stringify %)))
      (.catch #(error-handler %))))

(defn world [id handler error-handler]
  (-> @worlds
      (.findOne (clj->js {:where {:id id} :raw true}))
      (.then handler)
      (.catch #(error-handler %))))

(defn update-world! [world handler error-handler]
  (-> @worlds
      (.update (clj->js {:randomnumber (:randomnumber world)})
               (clj->js {:where {:id (:id world)} :raw true}))
      (.then #(handler (js->clj world)))
      (.catch #(error-handler %))))

#_(mount.core/start #'db #'worlds #'fortunes)
#_(all-fortunes println println)
#_(run-query println println)

(defn get-query-count
  "Parse provided string value of query count, clamping values to between 1 and 500."
  [queries]
  (let [n (js/parseInt queries)]                       ; default to 1 on parse failure
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
  (let [query-count (get-query-count queries)]
    (repeatedly query-count #(world (unchecked-inc ^long (rand-int 10000)) handler error-handler))))

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
  [{:keys [id]} handler error-handler]
  (let [w {:id id :randomnumber (unchecked-inc (rand-int 9999))}]
    (try
      (update-world! w handler error-handler)
      (catch js/Error e
        (error-handler e)))
    w))

(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries handler error-handler]
  (mapv set-random-number! (run-queries queries handler error-handler)))