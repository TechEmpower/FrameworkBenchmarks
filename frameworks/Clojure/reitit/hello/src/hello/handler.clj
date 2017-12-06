(ns hello.handler
  (:require [clojure.tools.cli :as cli]
            [immutant.web :as web]
            [reitit.ring :as ring]
            [jsonista.core :as j])
  (:gen-class))

(defn json-handler [_]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (j/write-value-as-string {:message "Hello, World!"})})

(def app
  (some-fn
    (ring/ring-handler
      (ring/router
        [["/json" json-handler]]))
    (constantly {:status 404})))

(defn -main [& args]

  (let [[{:keys [help port]} _ banner]
        (cli/cli args
                 ["-p" "--port" "Server port"
                  :default 8080
                  :parse-fn #(Integer/parseInt %)]
                 ["-h" "--[no-]help"])]

    (when help
      (println banner)
      (System/exit 0))

    (web/run app {:host "0.0.0.0" :port port})))
