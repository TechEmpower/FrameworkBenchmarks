(ns io.github.kit-clj.te-bench.web.routes.bench
  (:require
    [io.github.kit-clj.te-bench.web.controllers.bench :as bench]
    [io.github.kit-clj.te-bench.web.middleware.default-headers :as default-headers]
    [io.github.kit-clj.te-bench.web.middleware.formats :as formats]
    [integrant.core :as ig]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [reitit.ring.middleware.parameters :as parameters]))

(derive :reitit.routes/bench :reitit/routes)

;; Routes
(defn bench-routes [{:keys [db-conn cache]}]
  [["/json" {:get bench/json-handler}]
   ["/plaintext" {:get bench/plaintext-handler}]
   ["/db" {:get (partial bench/db-handler db-conn)}]
   ["/queries" {:get (partial bench/multi-db-handler db-conn)}]
   ["/updates" {:get (partial bench/update-db-handler db-conn)}]
   ["/cached-queries" {:get (partial bench/cached-query-handler db-conn cache)}]
   ["/fortunes" {:get (partial bench/fortune-handler db-conn)}]])

(defmethod ig/init-key :reitit.routes/bench
  [_ {:keys [base-path]
      :or   {base-path ""}
      :as   opts}]
  [base-path
   {:muuntaja   formats/instance
    :middleware [;; query-params & form-params
                 parameters/parameters-middleware
                 ;; encoding response body
                 muuntaja/format-response-middleware
                 ;; default header middleware
                 default-headers/default-headers-middleware]}
   (bench-routes opts)])
