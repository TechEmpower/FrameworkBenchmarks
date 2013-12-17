(ns hello.routes.home
  (:use compojure.core
        hello.models.db)
  (:require [hello.views.layout :as layout]
            [noir.response :as response]))

(defroutes home-routes
  (GET "/" [] "Hello, World!")
  (GET "/plaintext" []
       {:status 200
        :headers {"Content-Type" "text/plain; charset=utf-8"}
        :body "Hello, World!"})
  (GET "/json" [] (response/json {:message "Hello, World!"}))
  (GET "/db/:queries" [queries] (response/json (run-queries (get-query-count queries))))
  (GET "/dbraw/:queries" [queries] (response/json (run-queries-raw (get-query-count queries))))  
  (GET "/fortune" [] (layout/render "home.html" {:messages (get-fortunes)})))

