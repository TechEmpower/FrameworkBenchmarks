(ns ring-http-exchange.input-stream-handler
  (:require
    [jj.majavat :as majavat]
    [jj.majavat.renderer :refer [->InputStreamRenderer]]
    [ring-http-exchange.model :as model]))

(defrecord Response [body status headers])

(def ^:private ^:const fortune-headers {"Server"       "ring-http-exchange"
                                        "Content-Type" "text/html; charset=UTF-8"})

(def ^:private render-fortune (majavat/build-html-renderer "fortune.html"
                                                           {:renderer (->InputStreamRenderer)}))

(defn create-callback [respond]
  (fn [fortune-data]
    (respond
      (Response.
        (render-fortune {:messages
                         (sort-by :message
                                  (conj fortune-data {:id      0
                                                      :message "Additional fortune added at request time."}))})
        200
        fortune-headers))))

(defn get-handler [data-source]
  (fn [req respond raise]
    (if (.equals "/fortunes" (req :uri))
      (model/vertx-query-fortunes data-source (create-callback respond) raise)
      (Response. model/hello-world 200 {}))))

