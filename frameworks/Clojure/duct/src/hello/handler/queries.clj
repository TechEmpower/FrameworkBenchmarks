(ns hello.handler.queries
  (:require [integrant.core :as ig]
            [ataraxy.response :as response]
            [hello.boundary.world-db :as world-db]))

; taken from the luminus sample
(defn query-count
  "Parse provided string value of query count, clamping values to between 1 and 500."
  [^String queries]
  (let [n ^long (try (Integer/parseInt queries)
                     (catch Exception _ 1))] ; default to 1 on parse failure
    (cond
      (< ^long n 1) 1
      (> ^long n 500) 500
      :else n)))

(defmethod ig/init-key :hello.handler/queries [_ {:keys [db]}]
  (fn [request]
    [::response/ok
     (let [queries (get-in request [:params :queries] "1")
           num (query-count queries)]
       (world-db/make-multiple-queries db num))]))
