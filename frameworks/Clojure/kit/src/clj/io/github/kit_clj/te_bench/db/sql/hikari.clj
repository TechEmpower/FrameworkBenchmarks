(ns io.github.kit-clj.te-bench.db.sql.hikari
  (:require
    [integrant.core :as ig]
    [kit.edge.db.sql.hikari]))

(defmethod ig/prep-key :db.sql/hikari-connection
  [_ config]
  (let [cpus (.availableProcessors (Runtime/getRuntime))]
    (assoc config :maximum-pool-size (* 8 cpus))))
