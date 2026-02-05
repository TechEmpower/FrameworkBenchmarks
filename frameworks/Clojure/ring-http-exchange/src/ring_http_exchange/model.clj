(ns ring-http-exchange.model
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [jsonista.core :as json]))

(def ^:const hello-world "Hello, World!")
(def ^:const fortune-query ["SELECT * FROM fortune"])
(def ^:const jdbc-opts {:builder-fn rs/as-unqualified-maps})
(defn hello-json []
  (json/write-value-as-string {:message hello-world}))

(defn fortunes-context [data-source]
  {:messages (as-> (jdbc/execute! data-source fortune-query jdbc-opts) fortunes
                   (conj fortunes {:id 0 :message "Additional fortune added at request time."})
                   (sort-by :message fortunes))})