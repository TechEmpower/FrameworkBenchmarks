(ns hello.boundary.fortune-db
  (:require [duct.database.sql]
            [duct.database.mongodb.monger]
            [clojure.java.jdbc :as jdbc]
            [monger.collection :as mc]))

(defprotocol Fortune
  (get-all [db]))

(extend-protocol Fortune
  duct.database.sql.Boundary
  (get-all [{:keys [spec]}]
    (jdbc/query spec ["select * from fortune"]))

  duct.database.mongodb.monger.Boundary
  (get-all [{:keys [db]}]
    (mc/find-maps db "fortune")))
