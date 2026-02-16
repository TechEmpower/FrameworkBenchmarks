(ns ring-http-exchange.input-stream-handler
  (:require
    [jj.majavat :as majavat]
    [jj.majavat.renderer :refer [->InputStreamRenderer]]
    [ring-http-exchange.model :as model])
  (:import (java.io ByteArrayInputStream)))

(defrecord Response [body status headers])

(def ^:private hello-world-bytes (.getBytes "Hello, World!"))
(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})

(def ^:private render-fortune (majavat/build-html-renderer "fortune.html"
                                                           {:renderer (->InputStreamRenderer)}))

(defn get-handler [data-source]
  (fn [req]
    (case (req :uri)
      "/fortunes" (Response. (render-fortune (model/fortunes-body data-source)) 200 fortune-headers)
      (Response. (ByteArrayInputStream. hello-world-bytes) 200 {"Server"       "ring-http-exchange"
                                                                "Content-Type" "text/plain"}))))

