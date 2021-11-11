(ns hello.handler.single-query-mongodb
  (:require [integrant.core :as ig]
            [ataraxy.response :as response]
            [hello.boundary.world-db :as world-db]))

(defmethod ig/init-key :hello.handler/single-query-mongodb [_ {:keys [db]}]
  (fn [{[_] :ataraxy/result}]
    [::response/ok (world-db/make-single-query db)]))