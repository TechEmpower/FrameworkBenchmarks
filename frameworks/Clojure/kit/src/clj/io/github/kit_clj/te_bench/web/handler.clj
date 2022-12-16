(ns io.github.kit-clj.te-bench.web.handler
  (:require
    [integrant.core :as ig]
    [reitit.ring :as ring]
    ))

(defmethod ig/init-key :handler/ring
  [_ {:keys [router]}]
  (ring/ring-handler
    router
    (ring/routes
      ;; Handle trailing slash in routes - add it + redirect to it
      ;; https://github.com/metosin/reitit/blob/master/doc/ring/slash_handler.md
      (ring/redirect-trailing-slash-handler))
    {:inject-match?  false
     :inject-router? false}))

(defmethod ig/init-key :router/routes
  [_ {:keys [routes]}]
  (apply conj [] routes))

(defmethod ig/init-key :router/core
  [_ {:keys [routes] :as opts}]
  (ring/router ["" opts routes]))
