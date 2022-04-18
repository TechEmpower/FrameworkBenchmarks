(ns hello.handler
  (:require
   [aleph.http        :as http]
   [aleph.netty       :as netty]
   [byte-streams      :as bs]
   [clojure.tools.cli :as cli]
   [jsonista.core     :as json])
  (:gen-class))

(def plaintext-response
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body (bs/to-byte-array "Hello, World!")})

(def json-response
  {:status 200
   :headers {"Content-Type" "application/json"}})

(defn handler [req]
  (let [uri (:uri req)]
    (cond
      (.equals "/plaintext" uri) plaintext-response
      (.equals "/json" uri)      (assoc json-response
                                        :body (json/write-value-as-bytes {:message "Hello, World!"}))
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

    (netty/leak-detector-level! :disabled)
    (http/start-server handler {:port port, :executor :none})))
