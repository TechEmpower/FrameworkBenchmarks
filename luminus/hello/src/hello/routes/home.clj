(ns hello.routes.home
  (:use compojure.core
        hello.models.db)
  (:require [hello.views.layout :as layout]
            [noir.response :as response]))

(defroutes home-routes
  (GET "/" [] "Hello, World!")
  (GET "/json" [] (response/json {:message "Hello, World!"}))
  (GET "/db" [] (response/json (first (run-queries 1))))
  (GET "/db/:queries" [queries] (response/json (run-queries (get-query-count queries))))
  (GET "/dbraw" [] (response/json (first (run-queries-raw 1))))
  (GET "/dbraw/:queries" [queries] (response/json (run-queries-raw (get-query-count queries))))  
  (GET "/fortune" [] (layout/render "home.html" {:messages (get-fortunes)})))

