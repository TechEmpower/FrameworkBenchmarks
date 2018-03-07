(ns hello.middleware
  (:require
    [macchiato.util.response :as r]))

(defn wrap-defaults [handler]
  (fn [req res raise]
    (handler req #(res (assoc-in % [:headers "Server"] "macchiato")) raise)))


