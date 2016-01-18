(ns hello.core
  (:require [hello.handler :refer [app init destroy]]
            [luminus.repl-server :as repl]
            [luminus.http-server :as http]
            [hello.db.migrations :as migrations]
            [environ.core :refer [env]])
  (:gen-class))

(defn parse-port [port]
  (when port
    (cond
      (string? port) (Integer/parseInt port)
      (number? port) port
      :else          (throw (Exception. (str "invalid port value: " port))))))

(defn http-port [port]
  (parse-port (or port (env :port) 3000)))

(defn stop-app []
  (repl/stop)
  (http/stop destroy)
  (shutdown-agents))

(defn start-app
  "e.g. lein run 3000"
  [[port]]
  (let [port (http-port port)]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app))
    (when-let [repl-port (env :nrepl-port)]
      (repl/start {:port (parse-port repl-port)}))
    (http/start {:handler app
                 :init    init
                 :port    port})))

(defn -main [& args]
  (cond
    (some #{"migrate" "rollback"} args)
    (do (migrations/migrate args) (System/exit 0))
    :else
    (start-app args)))
  
