(ns hello.handler
  (:require
    [byte-streams :as bs]
    [clojure.tools.cli :as cli]
    [aleph.http :as http]
    [jsonista.core :as json]
    [clj-tuple :as t])
  (:gen-class))

(defn handler [req]
  (let [uri (:uri req)]
    (cond
      (.equals "/plaintext" uri) {:status 200
                                  :headers {"content-type" "text/plain; charset=utf-8"}
                                  :body (bs/to-byte-array "Hello, World!")}
      (.equals "/json" uri) {:status 200
                             :headers {"content-type" "application/json"}
                             :body (json/write-value-as-bytes {:message "Hello, World!"})}
      :else {:status 404})))

;;;

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

    (aleph.netty/leak-detector-level! :disabled)
    (http/start-server handler {:port port, :executor :none})))
