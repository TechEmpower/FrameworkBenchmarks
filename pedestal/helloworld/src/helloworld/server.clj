(ns helloworld.server
  (:gen-class) ; for -main method in uberjar
  (:require [io.pedestal.service-tools.server :as server]
            [helloworld.service :as service]
            [io.pedestal.service.http :as bootstrap]
            [io.pedestal.service-tools.dev :as dev]))

(defn create-server
  "Standalone dev/prod mode."
  [& [opts]]
  (alter-var-root #'service-instance
                  (constantly (bootstrap/create-server (merge service/service opts)))))

(defn run-dev
  "The entry-point for 'lein run-dev'"
  [& args]
  (dev/init service/service #'service/routes)
  (apply dev/-main args))

;; To implement your own server, copy io.pedestal.service-tools.server and
;; customize it.

(defn -main
  "The entry-point for 'lein run'"
  [& args]
  (server/init service/service)
  (apply server/-main args))

;; Fns for use with io.pedestal.servlet.ClojureVarServlet

(defn servlet-init [this config]
  (server/init service/service)
  (server/servlet-init this config))

(defn servlet-destroy [this]
  (server/servlet-destroy this))

(defn servlet-service [this servlet-req servlet-resp]
  (server/servlet-service this servlet-req servlet-resp))
