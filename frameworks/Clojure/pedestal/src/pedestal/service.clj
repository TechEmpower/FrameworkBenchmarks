(ns pedestal.service
  (:require [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]
            [ring.util.response :as ring-resp]
            [clojure.data.json :as json]))

(defn about-page
  [request]
  (ring-resp/response (format "Clojure %s - served from %s"
                              (clojure-version)
                              (route/url-for ::about-page))))

(defn json-serialization
  [request]
  (bootstrap/json-response {:message "Hello, World!"}))

(defn plaintext
  [request]
  (ring-resp/response "Hello, World!"))




(defroutes routes
  [[
  [  "/json" {:get json-serialization}]
  [  "/plaintext" {:get plaintext}]]])

;; How the server will look, not the code to start it up
(def service {:env :prod
              ::bootstrap/routes routes
              ::bootstrap/resource-path "/public"
              ::bootstrap/type :jetty
              ::bootstrap/port 8080})
