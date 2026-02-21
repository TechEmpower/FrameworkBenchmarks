(ns ring-http-exchange.string-handler
  (:require
    [jj.majavat :as majavat]
    [ring-http-exchange.model :as model]))

(defrecord Response [body status headers])
(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})
(def ^:private ^:const json-headers {"Server"       "ring-http-exchange"
                                     "Content-Type" "application/json"})
(def ^:private ^:const plain-text-headers {"Server"       "ring-http-exchange"
                                           "Content-Type" "text/plain"})

(def ^:private render-fortune (majavat/build-html-renderer "fortune.html"))

(defn get-handler [data-source]
  (fn [req]
    (case (req :uri)
      "/plaintext" (Response. model/hello-world 200 plain-text-headers)
      "/json" (Response. (model/json-body) 200 json-headers)
      "/fortunes" (Response. (render-fortune (model/fortunes-body  data-source)) 200 fortune-headers)
      (Response. model/hello-world 200 {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/plain"}))))

