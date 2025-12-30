(ns io.github.kit-clj.te-bench.db.sql.hikari
  (:require
    [integrant.core :as ig]
    [kit.edge.db.sql.hikari]))

(defmethod ig/prep-key :db.sql/hikari-connection
  [_ config]
  (assoc config :maximum-pool-size 520))
