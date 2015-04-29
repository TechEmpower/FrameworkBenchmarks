(ns hello.core
  (:require [hello.handler :refer [app init]]
            [clojure.tools.cli :refer [cli]]
            [org.httpkit.server :refer [run-server]])
  (:gen-class))

(defn parse-port [s]
  "Convert stringy port number int. Defaults to 8080."
  (cond
    (string? s) (Integer/parseInt s)
    (instance? Integer s) s
    (instance? Long s) (.intValue ^Long s)
    :else 8080))

(defn start-server [{:keys [port]}]
  (let [cpu (.availableProcessors (Runtime/getRuntime))]
    ;; double worker threads should increase database access performance
    (init)
    (run-server app {:port port :thread (* 2 cpu)})
    (println (str "http-kit server listens at :" port))))

(defn -main [& args]
  (let [[options _ banner]
        (cli args
             ["-p" "--port" "Port to listen" :default 8080 :parse-fn parse-port]
             ["--[no-]help" "Print this help"])]
    (when (:help options)
          (println banner)
          (System/exit 0))
    (start-server options)))
