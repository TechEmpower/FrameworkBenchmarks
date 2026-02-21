(ns ring-http-exchange.model
  (:require [jj.sql.boa :as boa]
            [jj.sql.boa.query.next-jdbc :as next-jdbc-adapter]
            [jsonista.core :as json]))

(def ^:const hello-world "Hello, World!")
(def query-fortunes (boa/build-query (next-jdbc-adapter/->NextJdbcAdapter) "fortune.sql"))

(defn json-body []
  (json/write-value-as-string {:message hello-world}))

(defn fortunes-body [data-source]
  {:messages (as-> (query-fortunes data-source) fortunes
                   (conj fortunes {:id      0
                                   :message "Additional fortune added at request time."})
                   (sort-by :message fortunes))})

