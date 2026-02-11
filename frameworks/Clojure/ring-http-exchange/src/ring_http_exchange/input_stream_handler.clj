(ns ring-http-exchange.input-stream-handler
  (:require
    [jj.majavat :as majavat]
    [jj.majavat.renderer :refer [->InputStreamRenderer]]
    [jj.sql.boa :as boa])
  (:import (java.io ByteArrayInputStream)))

(defrecord Response [body status headers])

(def query-fortunes (boa/build-query (boa/->NextJdbcAdapter) "fortune.sql"))

(def ^:private hello-world-bytes (.getBytes "Hello, World!"))

(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})

(def ^:private render-fortune (majavat/build-html-renderer "fortune.html"
                                                           {:renderer (->InputStreamRenderer)}))

(defn- get-body [datasource]
  (let [context (as-> (query-fortunes datasource) fortunes
                      (conj fortunes {:id      0
                                      :message "Additional fortune added at request time."})
                      (sort-by :message fortunes))]
    (render-fortune {:messages context})))

(defn get-handler [data-source]
  (fn [req]
    (case (req :uri)
      "/fortunes" (Response. (get-body data-source) 200 fortune-headers)
      (Response. (ByteArrayInputStream. hello-world-bytes) 200 {"Server"       "ring-http-exchange"
                                                                "Content-Type" "text/plain"}))))

