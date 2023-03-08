(ns io.github.kit-clj.te-bench.web.handler
  (:require
    [integrant.core :as ig]
    [reitit.ring :as ring]
    ))

(defmethod ig/init-key :handler/ring
  [_ {:keys [router]}]
  (ring/ring-handler
    router
    nil
    {:inject-match?  false
     :inject-router? false}))

(defmethod ig/init-key :router/routes
  [_ {:keys [routes]}]
  (apply conj [] routes))

(defmethod ig/init-key :router/core
  [_ {:keys [routes] :as opts}]
  (ring/router ["" opts routes]))
