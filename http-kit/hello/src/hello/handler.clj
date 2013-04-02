(ns hello.handler
  (:gen-class)
  (:use compojure.core
        ring.middleware.json
        org.httpkit.server
        [clojure.tools.cli :only [cli]]
        ring.util.response)
  (:require [compojure.handler :as handler]
            [org.httpkit.dbcp :as db]
            [compojure.route :as route]))

;;; convert to int
(defn to-int [s] (cond
                  (string? s) (Integer/parseInt s)
                  (instance? Integer s) s
                  (instance? Long s) (.intValue ^Long s)
                  :else 0))

;; Query a random World record from the database
(defn get-world []
  (let [id (inc (rand-int 9999))] ; Num between 1 and 10,000
    (db/query "select id randomNumber from world where id = ?" id)))

;; Run the specified number of queries, return the results
(defn run-queries [queries]
  (vec ; Return as a vector
   (flatten ; Make it a list of maps
    (take
     queries ; Number of queries to run
     (repeatedly get-world)))))

;; Define route handlers
(defroutes app-routes
  (GET "/http-kit/" [] "Hello, World!")
  (GET "/http-kit/json" [] (response {:message "Hello, World!"}))
  (GET "/http-kit/db/:queries" [queries]
       (response (run-queries (Integer/parseInt queries))))
  (route/not-found "Not Found"))


(defn start-server [{:keys [port worker db-host]}]
  (db/use-database! (str "jdbc:mysql://" db-host "localhost/hello_world")
                    "benchmarkdbuser"
                    "benchmarkdbpass")
  ;; Format responses as JSON
  (let [handler (wrap-json-response app-routes)]
    (run-server handler {:port port :worker worker})))


(defn -main [& args]
  (let [[options _ banner]
        (cli args
             ["-p" "--port" "Port to listen" :default 8080 :parse-fn to-int]
             ["--worker" "Http worker thread count" :default 6 :parse-fn to-int]
             ["--db-host" "MySQL database host" :default "localhost"]
             ["--[no-]help" "Print this help"])]
    (when (:help options) (println banner) (System/exit 0))
    (start-server options)))
