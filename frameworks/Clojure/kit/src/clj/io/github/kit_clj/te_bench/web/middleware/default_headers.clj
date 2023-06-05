(ns io.github.kit-clj.te-bench.web.middleware.default-headers
  (:require
    [ring.util.http-response :as http-response]))

(def default-headers-middleware
  "Adds default headers required for TechEmpower benchmarks"
  {:name    ::default-headers
   :compile (fn [_route-data _opts]
              (fn [handler]
                (fn
                  ([request]
                   (http-response/header (handler request) "Server" "Kit"))
                  ([request respond raise]
                   (handler request #(respond (http-response/header % "Server" "Kit")) raise)))))})