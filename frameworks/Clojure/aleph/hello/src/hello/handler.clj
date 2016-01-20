(ns hello.handler
  (:require
    [byte-streams :as bs]
    [clojure.tools.cli :as cli]
    [aleph.http :as http]
    [cheshire.core :as json]
    [clj-tuple :as t])
  (:gen-class))

(def plaintext-response
  (t/hash-map
    :status 200
    :headers (t/hash-map "content-type" "text/plain; charset=utf-8")
    :body (bs/to-byte-array "Hello, World!")))

(def json-response
  (t/hash-map
    :status 200
    :headers (t/hash-map "content-type" "application/json")))

(defn handler [{:keys [uri] :as req}]
  (cond
    (= "/plaintext" uri) plaintext-response
    (= "/json" uri) (assoc json-response
                      :body (json/encode (t/hash-map :message "Hello, World!")))
    :else {:status 404}))

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
