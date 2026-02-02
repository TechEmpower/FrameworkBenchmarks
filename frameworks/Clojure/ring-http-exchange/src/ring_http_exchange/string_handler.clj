(ns ring-http-exchange.string-handler
  (:require
    [jj.majavat :as majavat]
    [jj.sql.boa :as boa]
    [jsonista.core :as json]))

(defrecord Response [body status headers])

(def query-fortunes (boa/build-query (boa/->NextJdbcAdapter) "fortune.sql"))

(def ^:private ^:const hello-world "Hello, World!")

(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})
(def ^:private ^:const json-headers {"Server"       "ring-http-exchange"
                                     "Content-Type" "application/json"})
(def ^:private ^:const plain-text-headers {"Server"       "ring-http-exchange"
                                           "Content-Type" "text/plain"})

(def ^:private render-fortune (majavat/build-html-renderer "fortune.html"))

(defn- get-body [datasource]
  (let [context (as-> (query-fortunes datasource) fortunes
                      (conj fortunes {:id      0
                                      :message "Additional fortune added at request time."})
                      (sort-by :message fortunes))]
    (render-fortune {:messages context})))

(defn get-handler [data-source]
  (fn [req]
    (case (req :uri)
      "/plaintext" (Response. hello-world 200 plain-text-headers)
      "/json" (Response. (json/write-value-as-string {:message hello-world}) 200 json-headers)
      "/fortunes" (Response. (get-body data-source) 200 fortune-headers)
      (Response. hello-world 200 {"Server"       "ring-http-exchange"
                                  "Content-Type" "text/plain"}))))

