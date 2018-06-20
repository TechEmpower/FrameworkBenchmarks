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

; Create a specific handler for HTTP 200 but patching the response header
; so that it is "text/plain" and not "text/html" as inferred by default
; in case the response is a simple String.
;(defmethod handler/sync-default ::ok.plain-text [{[_ body] :ataraxy/result}]
;  (let [r (-> (ataraxy-resp/->response body) (resp/status 200))]
;    (update-in r [:headers "Content-Type"] (constantly "text/plain"))))

;(defmethod ig/init-key :hello.handler/plaintext [_ options]
;  (fn [{[_] :ataraxy/result}]
;    [::ok.plain-text "Hello, World!"]))
