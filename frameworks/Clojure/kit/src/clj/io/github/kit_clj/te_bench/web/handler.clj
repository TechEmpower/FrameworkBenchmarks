(ns io.github.kit-clj.te-bench.web.handler
  (:require
    [integrant.core :as ig]
    [ring.util.http-response :as http-response]
    [reitit.ring :as ring]
    ))

(defmethod ig/init-key :handler/ring
  [_ {:keys [router]}]
  (ring/ring-handler
    router
    (ring/routes
      ;; Handle trailing slash in routes - add it + redirect to it
      ;; https://github.com/metosin/reitit/blob/master/doc/ring/slash_handler.md
      (ring/redirect-trailing-slash-handler)
      (ring/create-resource-handler {:path "/"})
      (ring/create-default-handler
        {:not-found
         (constantly (-> {:status 404, :body "Page not found"}
                         (http-response/content-type "text/html")))
         :method-not-allowed
         (constantly (-> {:status 405, :body "Not allowed"}
                         (http-response/content-type "text/html")))
         :not-acceptable
         (constantly (-> {:status 406, :body "Not acceptable"}
                         (http-response/content-type "text/html")))}))
    {:inject-match?  false
     :inject-router? false}))

(defmethod ig/init-key :router/routes
  [_ {:keys [routes]}]
  (apply conj [] routes))

(defmethod ig/init-key :router/core
  [_ {:keys [routes] :as opts}]
  (ring/router ["" opts routes]))
