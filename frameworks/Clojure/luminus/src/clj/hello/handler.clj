(ns hello.handler
  (:require [compojure.core :refer [routes]]
            [compojure.route :as route]
            [hello.layout :refer [error-page]]
            [hello.routes.home :refer [default-routes io-routes]]))

(def default-handler
  (routes
    #'default-routes
    (route/not-found
      (:body
        (error-page {:status 404
                     :title "page not found"})))))

(def io-handler
  (routes
    #'io-routes
    (route/not-found
      (:body
        (error-page {:status 404
                     :title "page not found"})))))
