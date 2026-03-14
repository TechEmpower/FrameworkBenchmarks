(ns io.github.kit-clj.te-bench.web.controllers.bench
  (:require
    [clojure.core.cache :as cache]
    [hiccup.page :as hp]
    [hiccup.util :as hu]
    [jj.majavat :as majavat]
    [jj.majavat.renderer :refer [->StringRenderer]]
    [jj.sql.boa :as boa]
    [next.jdbc :as jdbc]
    [next.jdbc.result-set :as rs]
    [ring.util.http-response :as http-response]
    [selmer.parser :as parser]))

;; -----------
;; Utils
;; -----------

(def ^:const HELLO_WORLD "Hello, World!")
(def ^:const MAX_ID_ZERO_IDX 9999)
(def ^:const CACHE_TTL (* 24 60 60))
(def ^:private render-fortune (majavat/build-html-renderer "html/majavat-fortunes.html"
                                                           {:renderer (->StringRenderer)}))

(defn render-hiccup-fortune [fortunes]
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

(def query-fortunes (boa/build-query (boa/->NextJdbcAdapter) "sql/fortunes.sql"))
(def selmer-opts {:custom-resource-path (clojure.java.io/resource "html")})

(defn selmer-html-response
  [template & [params]]
  (-> (parser/render-file template params selmer-opts)
      (http-response/ok)
      (http-response/content-type "text/html; charset=utf-8")))

(defn majavat-html-response
  [context]
  (-> (render-fortune context)
      (http-response/ok)
      (http-response/content-type "text/html; charset=utf-8")))

(defn hiccup-html-response
  [body]
  (-> (http-response/ok body)
      (http-response/content-type "text/html; charset=utf-8")))

(defn rand-id
  [n]
  (inc (rand-int n)))

;; From Luminus benchmark
(defn query-count
  "Parse provided string value of query count, clamping values to between 1 and 500."
  [^String queries]
  (let [n (try (Integer/parseInt queries)
               (catch Exception _ 1))]                      ; default to 1 on parse failure
    (cond
      (< n 1) 1
      (> n 500) 500
      :else n)))

(defn range-from-req
  [request]
  (range (query-count (get-in request [:query-params "queries"] "1"))))

;; -----------
;; Queries
;; -----------

(def jdbc-opts {:builder-fn rs/as-unqualified-lower-maps})

(defn db-query-world!
  [db-conn]
  (jdbc/execute-one! db-conn ["select * from \"World\" where id = ?" (rand-id MAX_ID_ZERO_IDX)]
                     jdbc-opts))

(defn db-multi-query-world!
  "Queries multiple times outside context of a tx"
  [db-conn request]
  (reduce
    (fn [out _]
      (conj out (db-query-world! db-conn)))
    []
    (range-from-req request)))

(defn update-world!
  [db-conn id rand-number]
  (jdbc/execute-one! db-conn
                     ["update \"World\" set randomNumber = ? where id = ? returning *;" rand-number id]
                     jdbc-opts))

;; -----------
;; Cache
;; -----------

(defn cache-lookup-or-add
  [cache key lookup-fn ttl]
  (or (cache/lookup cache key)
      (let [value (lookup-fn)]
        (cache/miss cache key {:val value :ttl ttl})
        value)))

;; -----------
;; Handlers
;; -----------

(defn json-handler
  [_]
  (http-response/ok {:message HELLO_WORLD}))

(defn plaintext-handler
  [_]
  (-> (http-response/ok HELLO_WORLD)
      (http-response/content-type "text/plain")))

(defn db-handler
  [db-conn _request]
  (http-response/ok (db-query-world! db-conn)))

(defn multi-db-handler
  [db-conn request]
  (http-response/ok (db-multi-query-world! db-conn request)))

(defn update-db-handler
  [db-conn request]
  (let [items (db-multi-query-world! db-conn request)]
    (http-response/ok
      (mapv
        (fn [{:keys [id]}]
          (update-world! db-conn id (rand-id MAX_ID_ZERO_IDX)))
        items))))

(defn cached-query-handler
  [db-conn cache request]
  (http-response/ok
    (reduce
      (fn [out _]
        (let [id (rand-id MAX_ID_ZERO_IDX)]
          (conj out
                (cache-lookup-or-add cache
                                     id
                                     #(jdbc/execute-one! db-conn ["select * from \"World\" where id = ?;" id] jdbc-opts)
                                     CACHE_TTL))))
      []
      (range-from-req request))))

(defn selmer-fortune-handler
  [db-conn _request]
  (as-> (jdbc/execute! db-conn ["select * from \"Fortune\";"] jdbc-opts) fortunes
        (conj fortunes {:id 0 :message "Additional fortune added at request time."})
        (sort-by :message fortunes)
        (selmer-html-response "fortunes.html" {:messages fortunes})))

(defn majavat-fortune-handler
  [db-conn _request]
  (as-> (query-fortunes db-conn) fortunes
        (conj fortunes {:id 0 :message "Additional fortune added at request time."})
        (sort-by :message fortunes)
        (majavat-html-response {:messages fortunes})))

(defn hiccup-fortune-handler
  [db-conn _request]
  (as-> (query-fortunes db-conn) fortunes
        (conj fortunes {:id 0 :message "Additional fortune added at request time."})
        (sort-by :message fortunes)
        (render-hiccup-fortune fortunes)
        (hiccup-html-response fortunes)))
