(ns hello.handler
  (:require
   [aleph.http              :as http]
   [aleph.netty             :as netty]
   [byte-streams            :as bs]
   [clj-async-profiler.core :as prof]
   [hiccup.page             :as hp]
   [hiccup.util             :as hu]
   [jsonista.core           :as json]
   [manifold.deferred       :as d]
   [porsas.async            :as async])
  (:import (clojure.lang IDeref)
           (io.netty.channel ChannelOption)
           (io.netty.buffer PooledByteBufAllocator)
           (java.util.function Supplier)
           (java.util.concurrent ThreadLocalRandom)
           (porsas.async Context))
  (:gen-class))

(def plaintext-response
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body (bs/to-byte-array "Hello, World!")})

(def json-response
  {:status 200
   :headers {"Content-Type" "application/json"}})

(def html-response
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}})

(def db-spec
  {:uri "postgresql://tfb-database:5432/hello_world"
   :user "benchmarkdbuser"
   :password "benchmarkdbpass"
   :size 1})

(defmacro thread-local [& body]
  `(let [tl# (ThreadLocal/withInitial (reify Supplier (get [_] ~@body)))]
     (reify IDeref (deref [_] (.get tl#)))))

(def pool
  "PostgreSQL pool of connections (`PgPool`)."
  (thread-local (async/pool db-spec)))

(defn random
  "Generate a random number between 1 and 10'000."
  []
  (unchecked-inc (.nextInt (ThreadLocalRandom/current) 10000)))

(defn sanitize-queries-param
  "Sanitizes the `queries` parameter. Clamps the value between 1 and 500.
  Invalid (string) values become 1."
  [request]
  (let [queries (-> request
                    :query-string
                    (subs 8))
        n (try (Integer/parseInt queries)
               (catch Exception _ 1))] ; default to 1 on parse failure
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))

(def ^Context
  query-mapper
  "Map each row into a record."
  (async/context {:row (async/rs->compiled-record)}))

(defn query-one-random-world
  "Query a random world on the database.
  Return a `CompletableFuture`."
  []
  (async/query-one query-mapper
                   @pool
                   ["SELECT id, randomnumber FROM world WHERE id=$1" (random)]))

(defn update-world
  "Update a world on the database.
  Return a `CompletableFuture`."
  [{:keys [randomNumber id]}]
  (async/query @pool
               ["UPDATE world SET randomnumber=$1 WHERE id=$2" randomNumber id]))

(defn run-queries
  "Run a number of `queries` on the database to fetch a random world.
  Return a `manifold.deferred`."
  [queries]
  (apply d/zip
         (take queries
               (repeatedly query-one-random-world))))

(defn query-fortunes
  "Query the fortunes on database.
  Return a `CompletableFuture`."
  []
  (async/query query-mapper @pool ["SELECT id, message from FORTUNE"]))

(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text.
  Return a `CompletableFuture` with the results."
  []
  (d/chain (query-fortunes)
           (fn [fortunes]
             (sort-by :message
                      (conj fortunes
                            {:id 0
                             :message "Additional fortune added at request time."})))))

(defn update-and-persist
  "Fetch a number of `queries` random world from the database.
  Compute a new `randomNumber` for each of them a return a `CompletableFuture`
  with the updated worlds."
  [queries]
  (d/chain' (run-queries queries)
            (fn [worlds]
              (let [worlds' (mapv #(assoc % :randomNumber (random)) worlds)]
                (d/chain' (apply d/zip (mapv update-world worlds'))
                          (fn [_] worlds'))))))

(defn fortunes-hiccup
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
      (.equals "/json" uri)      (assoc json-response
                                        :body (json/write-value-as-bytes {:message "Hello, World!"}))
      (.equals "/db" uri)        (-> (query-one-random-world)
                                     (d/chain (fn [world]
                                                (assoc json-response
                                                       :body (json/write-value-as-bytes world)))))
      (.equals "/queries" uri)   (-> (sanitize-queries-param req)
                                     (run-queries)
                                     (d/chain (fn [worlds]
                                                (assoc json-response
                                                       :body (json/write-value-as-bytes worlds)))))
      (.equals "/fortunes" uri)  (d/chain' (get-fortunes)
                                           fortunes-hiccup
                                           (fn [body]
                                             (assoc html-response :body body)))
      (.equals "/updates" uri)   (-> (sanitize-queries-param req)
                                     (update-and-persist)
                                     (d/chain (fn [worlds]
                                                (assoc json-response
                                                       :body (json/write-value-as-bytes worlds)))))
      :else {:status 404})))

;;;

(defn -main [& _]
  (netty/leak-detector-level! :disabled)
  (http/start-server handler {:port 8080
                              :raw-stream? true
                              :epoll? true
                              :executor :none
                              :bootstrap-transform (fn [bootstrap]
                                                     (.option bootstrap ChannelOption/ALLOCATOR PooledByteBufAllocator/DEFAULT)
                                                     (.childOption bootstrap ChannelOption/ALLOCATOR PooledByteBufAllocator/DEFAULT))
                              :pipeline-transform (fn [pipeline]
                                                    (.remove pipeline "continue-handler"))})
  ;; Uncomment to enable async-profiler
  #_
  (do
    (prof/profile-for 60
                      #_
                      {:transform (fn [s]
                                    (when-not (re-find #"(writev|__libc|epoll_wait|write|__pthread)" s)
                                      s))})
    (prof/serve-files 8081)))
