(ns hello.handler
  (:require [compojure.core :refer [routes wrap-routes]]
            [hello.layout :refer [error-page]]
            [hello.routes.home :refer [default-routes io-routes]]
            [compojure.route :as route]))

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
