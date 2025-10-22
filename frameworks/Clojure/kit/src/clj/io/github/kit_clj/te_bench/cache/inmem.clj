(ns io.github.kit-clj.te-bench.cache.inmem
  (:require
    [clojure.core.cache :as cache]
    [integrant.core :as ig]
    [next.jdbc :as jdbc]
    [next.jdbc.result-set :as rs]))

(defmethod ig/init-key :cache/inmem
  [_ {:keys [db-conn threshold]}]
  (cache/fifo-cache-factory
    (reduce
      (fn [out {:keys [id] :as obj}]
        (assoc out id obj))
      {}
      (jdbc/execute! db-conn ["select * from \"World\""] {:builder-fn rs/as-unqualified-lower-maps}))
    {:threshold threshold}))