(ns hello.handler
  (:require
    [aleph.http :as http]
    [aleph.netty :as netty]
    [hiccup.page :as hp]
    [hiccup.util :as hu]
    [jsonista.core :as json]
    [manifold.deferred :as d]
    [next.jdbc :as jdbc]
    [next.jdbc.connection :as connection]
    [next.jdbc.result-set :as rs])

  (:import (com.zaxxer.hikari HikariDataSource)
           (io.netty.buffer PooledByteBufAllocator)
           (io.netty.channel ChannelOption)
           (java.util.concurrent ThreadLocalRandom))
  (:gen-class))

(def jdbc-opts {:builder-fn rs/as-unqualified-maps})

(def db-spec
  {:jdbcUrl "jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"})

(def datasource
  (connection/->pool HikariDataSource db-spec))

(def plaintext-response
  {:status  200
   :headers {"Content-Type" "text/plain"}
   :body    (.getBytes "Hello, World!")})

(def json-response
  {:status  200
   :headers {"Content-Type" "application/json"}})

(def html-response
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}})


(defn- random
  "Generate a random number between 1 and 10'000."
  []
  (unchecked-inc (.nextInt (ThreadLocalRandom/current) 10000)))

(defn- sanitize-queries-param
  [request]
  (let [queries (-> request
                    :query-string
                    (subs 8))
        n (try (Integer/parseInt queries)
               (catch Exception _ 1))]                      ; default to 1 on parse failure
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))


(defn- query-one-random-world []
  (jdbc/execute-one! datasource
                     ["select * from \"World\" where id = ?;" (random)]
                     jdbc-opts))

(defn- update-world
  [{:keys [randomnumber  id]}]
  (jdbc/execute-one! datasource
                     ["update \"World\" set randomNumber = ? where id = ? returning *;" randomnumber id]
                     jdbc-opts))

(defn- run-queries
  "Run a number of `queries` on the database to fetch a random world.
  Return a `manifold.deferred`."
  [queries]
  (apply d/zip
         (take queries
               (repeatedly query-one-random-world))))


(defn query-fortunes []
  (jdbc/execute! datasource
                 ["select * from \"Fortune\";"]
                 jdbc-opts))

(defn- get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text.
  Return a `CompletableFuture` with the results."
  []
  (d/chain (query-fortunes)
           (fn [fortunes]
             (sort-by :message
                      (conj fortunes
                            {:id      0
                             :message "Additional fortune added at request time."})))))

(defn- update-and-persist [queries]
  (d/chain' (run-queries queries)
            (fn [worlds]
              (let [worlds' (mapv #(assoc % :randomnumber (random)) worlds)]
                (d/chain' (apply d/zip (mapv update-world worlds'))
                          (fn [_] worlds'))))))

(defn- fortunes-hiccup
  "Render the given fortunes to simple HTML using Hiccup."
  [fortunes]
  (hp/html5
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
         [:td (hu/escape-html (:message x))]])]]))

(defn handler
  "Ring handler representing the different tests."
  [req]
  (let [uri (:uri req)]
    (cond
      (.equals "/plaintext" uri) plaintext-response
      (.equals "/json" uri) (assoc json-response
                              :body (json/write-value-as-bytes {:message "Hello, World!"}))
      (.equals "/db" uri) (-> (query-one-random-world)
                              (d/chain (fn [world]
                                         (assoc json-response
                                           :body (json/write-value-as-bytes world)))))
      (.equals "/queries" uri) (-> (sanitize-queries-param req)
                                   (run-queries)
                                   (d/chain (fn [worlds]
                                              (assoc json-response
                                                :body (json/write-value-as-bytes worlds)))))
      (.equals "/fortunes" uri) (d/chain' (get-fortunes)
                                          fortunes-hiccup
                                          (fn [body]
                                            (assoc html-response :body body)))
      (.equals "/updates" uri) (-> (sanitize-queries-param req)
                                   (update-and-persist)
                                   (d/chain (fn [worlds]
                                              (assoc json-response
                                                :body (json/write-value-as-bytes worlds)))))
      :else {:body   "Not found"
             :status 404})))


(defn -main [& _]
  (netty/leak-detector-level! :disabled)
  (println "starting server on port 8080")
  (http/start-server handler {:port                8080
                              :raw-stream?         true
                              :executor            :none
                              :bootstrap-transform (fn [bootstrap]
                                                     (.option bootstrap ChannelOption/ALLOCATOR PooledByteBufAllocator/DEFAULT)
                                                     (.childOption bootstrap ChannelOption/ALLOCATOR PooledByteBufAllocator/DEFAULT))
                              :pipeline-transform  (fn [pipeline]
                                                     (.remove pipeline "continue-handler"))}))
