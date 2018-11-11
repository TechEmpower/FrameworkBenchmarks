(ns hello.handler.plaintext
  (:require [integrant.core :as ig]
            [ataraxy.handler :as handler]
            [ataraxy.response :as ataraxy-resp]
            [ring.util.response :as resp]))

; Create a specific init-key extending ::ok, patching the Content-Type header
; so that it equals "text/plain" instead of "application/octet-stream"
(defmethod ig/init-key :duct.handler.static/okplain [_ response]
  (let [val (ig/init-key :duct.handler.static/ok response)
        patched (update-in (val response) [:headers "Content-Type"] (constantly "text/plain"))]
    (constantly patched)))
