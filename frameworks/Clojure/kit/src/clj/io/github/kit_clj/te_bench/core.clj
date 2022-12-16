(ns io.github.kit-clj.te-bench.core
  (:require
    [clojure.tools.logging :as log]
    [integrant.core :as ig]
    [io.github.kit-clj.te-bench.config :as config]
    [io.github.kit-clj.te-bench.env :refer [defaults]]

    ;; Edges       
    [io.github.kit-clj.te-bench.cache.inmem]
    [io.github.kit-clj.te-bench.db.sql.hikari]
    [io.github.kit-clj.te-bench.web.handler]
    [kit.edge.server.undertow]

    ;; Routes
    [io.github.kit-clj.te-bench.web.routes.bench])
  (:gen-class))

;; log uncaught exceptions in threads
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      (log/error {:what :uncaught-exception
                  :exception ex
                  :where (str "Uncaught exception on" (.getName thread))}))))

(defonce system (atom nil))

(defn stop-app []
  ((or (:stop defaults) (fn [])))
  (some-> (deref system) (ig/halt!))
  (shutdown-agents))

(defn start-app [& [params]]
  ((or (:start params) (:start defaults) (fn [])))
  (->> (config/system-config (or (:opts params) (:opts defaults) {}))
       (ig/prep)
       (ig/init)
       (reset! system))
  (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app)))

(defn -main [& _]
  (start-app))
