(ns hello.handler.queries-mongodb
  (:require [integrant.core :as ig]
            [ataraxy.response :as response]
            [hello.boundary.world-db :as world-db]
            [hello.handler.queries :refer [query-count]]))

(defmethod ig/init-key :hello.handler/queries-mongodb [_ {:keys [db]}]
  (fn [request]
    [::response/ok
     (let [queries (get-in request [:params :queries] "1")
           num (query-count queries)]
       (world-db/make-multiple-queries db num))]))