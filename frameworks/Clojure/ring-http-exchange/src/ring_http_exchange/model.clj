(ns ring-http-exchange.model
  (:require [jj.sql.boa :as sync-boa]
            [jj.sql.async-boa :as async-boa]
            [jj.sql.boa.query.next-jdbc :as next-jdbc-adapter]
            [jj.sql.boa.query.vertx-pg :as vertx-adapter]
            [jj.sql.boa.query.next-jdbc-async :as async-next-jdbc-adapter]
            [jsonista.core :as json])
  (:import (java.util.concurrent Executors)))

(def executor (Executors/newVirtualThreadPerTaskExecutor))

(def ^:const hello-world "Hello, World!")
(def query-fortunes (sync-boa/build-query (next-jdbc-adapter/->NextJdbcAdapter) "fortune.sql"))
(def async-query-fortunes (async-boa/build-async-query (async-next-jdbc-adapter/->NextJdbcAdapter executor) "fortune.sql"))
(def vertx-query-fortunes (async-boa/build-async-query (vertx-adapter/->VertxPgAdapter) "fortune.sql"))

(defn json-body []
  (json/write-value-as-string {:message hello-world}))

(defn fortunes-body [data-source]
  {:messages (as-> (query-fortunes data-source) fortunes
                   (conj fortunes {:id      0
                                   :message "Additional fortune added at request time."})
                   (sort-by :message fortunes))})
