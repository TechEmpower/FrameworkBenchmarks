(ns hello.boundary.world-db
  (:require [duct.database.sql]
            [duct.database.mongodb.monger]
            [clojure.java.jdbc :as jdbc]
            [monger.collection :as mc]))

(defn- query [db]
  (first
    (jdbc/query db ["select * from world where id = ?" (inc (rand-int 10000))])))

(defn- mongo-query [db]
  (dissoc (mc/find-one-as-map db "world" {:id (inc (rand-int 10000))}) :_id))

(defprotocol World
  (make-single-query [db])
  (make-multiple-queries [db num]))

(extend-protocol World
  duct.database.sql.Boundary
  (make-single-query [{:keys [spec]}]
    (query spec))

  (make-multiple-queries [{:keys [spec]} num]
    (repeatedly num #(query spec))))

(extend-protocol World
  duct.database.mongodb.monger.Boundary
  (make-single-query [{:keys [db]}]
    (mongo-query db))

  (make-multiple-queries [{:keys [db]} num]
    (repeatedly num #(mongo-query db))))