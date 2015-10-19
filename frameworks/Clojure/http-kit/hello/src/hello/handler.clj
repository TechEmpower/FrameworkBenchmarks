(ns hello.handler
  (:gen-class)
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:use compojure.core
        ring.middleware.json
        org.httpkit.server
        [clojure.tools.cli :only [cli]]
        korma.db
        korma.core
        hiccup.core
        hiccup.util
        hiccup.page)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.util.response :as ring-resp]
            [clojure.data.json :as json]
            [clojure.java.jdbc :as jdbc]))


(defn parse-port [s] 
  "Convert stringy port number int. Defaults to 8080."
  (cond
    (string? s) (Integer/parseInt s)
    (instance? Integer s) s
    (instance? Long s) (.intValue ^Long s)
    :else 8080))


;; MySQL connection
(defdb mysql-db
  (mysql {
    :classname "com.mysql.jdbc.Driver"
    :subprotocol "mysql"
    :subname "//127.0.0.1:3306/hello_world"
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


(defn sanitize-queries-param
  "Sanitizes the `queries` parameter. Caps the value between 1 and 500.
  Invalid (stringy) values become 1"
  [queries]
  (let [n (try (Integer/parseInt queries)
               (catch Exception e 1))] ; default to 1 on parse failure
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))


(defn random-world
  "Query a random World record from the database"
  []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (select world
            (where {:id id }))))


(defn run-queries
  "Run query repeatedly -- Always returns an array"
  [queries]
  (flatten (take queries (repeatedly random-world))))

; Set up entity Fortune and the database representation
(defentity fortune
  (pk :id)
  (table :fortune)
  (entity-fields :id :message)
  (database mysql-db))


(defn get-all-fortunes
  "Query all Fortune records from the database."
  []
  (select fortune
          (fields :id :message)))


(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  []
  (sort-by #(:message %)
    (conj
      (get-all-fortunes)
      { :id 0 :message "Additional fortune added at request time." })))


(defn fortunes-hiccup
  "Render the given fortunes to simple HTML using Hiccup."
  [fortunes]
  (html5
   [:head
    [:title "Fortunes"]]
   [:body
    [:table
     [:tr
      [:th "id"]
      [:th "message"]]
     (for [x fortunes]
       [:tr
        [:td (:id x)]
        [:td (escape-html (:message x))]])
     ]]))


(defn update-and-persist
  "Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  (let [results (-> queries
                    (sanitize-queries-param)
                    (run-queries))]
    (for [w results]
      (update-in w [:randomNumber (inc (rand-int 9999))]
        (update world
                (set-fields {:randomNumber (:randomNumber w)})
                (where {:id [:id w]}))))
    results))


(def json-serialization
  "Test 1: JSON serialization"
  (ring-resp/response {:message "Hello, World!"}))


(def single-query-test
  "Test 2: Single database query"
  (ring-resp/response (first (run-queries 1))))


(defn multiple-queries-test
  "Test 3: Multiple database queries"
  [queries]
  (-> queries
      (sanitize-queries-param)
      (run-queries)
      (ring-resp/response)
      (ring-resp/content-type "application/json")))


(def fortune-test
  "Test 4: Fortunes"
  (->
    (get-fortunes)
    (fortunes-hiccup)
    (ring-resp/response)
    (ring-resp/content-type "text/html")))


(defn db-updates
  "Test 5: Database updates"
  [queries]
  (-> queries
      (update-and-persist)
      (ring-resp/response)))


(def plaintext
  "Test 6: Plaintext"
  (->
    (ring-resp/response "Hello, World!")
    (ring-resp/content-type "text/plain")))


;; Define route handlers
(defroutes app-routes
  (GET "/"                 [] "Hello, World!")
  (GET "/json"             [] json-serialization)
  (GET "/db"               [] single-query-test)
  (GET "/queries/:queries" [queries] (multiple-queries-test queries))
  (GET "/fortunes"         [] fortune-test)
  (GET "/updates/:queries" [queries] (db-updates queries))
  (GET "/plaintext"        [] plaintext)
  (route/not-found "Not Found"))


(defn start-server [{:keys [port]}]
  ;; Format responses as JSON
  (let [handler (wrap-json-response app-routes)
        cpu (.availableProcessors (Runtime/getRuntime))]
    ;; double worker threads should increase database access performance
    (run-server handler {:port port
                         :thread (* 2 cpu)})
    (println (str "http-kit server listens at :" port))))


(defn -main [& args]
  (let [[options _ banner]
        (cli args
             ["-p" "--port" "Port to listen" :default 8080 :parse-fn parse-port]
             ["--[no-]help" "Print this help"])]
    (when (:help options)
          (println banner)
          (System/exit 0))
    (start-server options)))
