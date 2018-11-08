(ns hello.handler.fortunes-mongodb
  (:require [integrant.core :as ig]
            [ataraxy.response :as response]
            [hello.boundary.fortune-db :as fortune-db]
            [hello.handler.fortunes :refer [prepare-fortunes fortunes-str]]))

(defmethod ig/init-key :hello.handler/fortunes-mongodb [_ {:keys [db]}]
  (fn [{[_] :ataraxy/result}]
    (let [fortunes (prepare-fortunes (fortune-db/get-all db))]
      [::response/ok (fortunes-str fortunes)])))
