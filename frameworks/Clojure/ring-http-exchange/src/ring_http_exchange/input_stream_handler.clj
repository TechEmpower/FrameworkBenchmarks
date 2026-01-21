(ns ring-http-exchange.input-stream-handler
  (:require
    [jj.majavat :as majavat]
    [jj.majavat.renderer :refer [->InputStreamRenderer]]
    [jj.majavat.renderer.sanitizer :refer [->Html]]
    [jj.sql.boa :as boa]
    [jsonista.core :as json])
  (:import (java.io ByteArrayInputStream)))

(defrecord Response [body status headers])

(def query-fortunes (boa/execute (boa/->NextJdbcAdapter) "fortune.sql"))

(def ^:private hello-world "Hello, World!")
(def ^:private hello-world-bytes (.getBytes "Hello, World!"))
(def ^:private ^:const additional-message {:id      0
                                           :message "Additional fortune added at request time."})
(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})
(def ^:private ^:const json-headers {"Server"       "ring-http-exchange"
                                     "Content-Type" "application/json"})
(def ^:private ^:const plain-text-headers {"Server"       "ring-http-exchange"
                                           "Content-Type" "text/plain"})

(def ^:private render-fortune (majavat/build-renderer "fortune.html"
                                                      {:renderer (->InputStreamRenderer
                                                                   {:sanitizer (->Html)})}))

(defn- get-body [datasource]
  (let [context (as-> (query-fortunes datasource) fortunes
                      (conj fortunes additional-message)
                      (sort-by :message fortunes))]
    (render-fortune {:messages context})))

(defn get-handler [data-source]
  (fn [req]
    (case (req :uri)
      "/fortunes" (Response. (get-body data-source) 200 fortune-headers)
      (Response. (ByteArrayInputStream. hello-world-bytes) 200 {"Server"       "ring-http-exchange"
                                                                "Content-Type" "text/plain"}))))

