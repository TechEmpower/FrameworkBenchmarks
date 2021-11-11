(ns hello.handler.fortunes
  (:require [integrant.core :as ig]
            [ataraxy.response :as response]
            [hello.boundary.fortune-db :as fortune-db]))

; copied from pedestal implementation
(defn prepare-fortunes
  [fortunes]
  (sort-by :message
           (conj fortunes
                 {:id 0 :message "Additional fortune added at request time."})))

(def ^String base-fortune-pre "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>")
(def ^String base-fortune-post "</table></body></html>")
(defn fortunes-str
  "The HTML bit for this is very very small;
  Opt to create the HTML string by hand in a tight loop rather than using Hiccup"
  [fortunes]
  (let [sb (StringBuilder. ^String base-fortune-pre)]
    (doseq [{:keys [id message]} fortunes]
      (.append sb "<tr><td>")
      (.append sb (str (int id)))
      (.append sb "</td><td>")
      (dotimes [c-idx (count message)]
        (let [c (.charAt ^String message c-idx)]
          (case c
            \& (.append sb "&amp;")
            \" (.append sb "&quot;")
            \' (.append sb "&apos;")
            \< (.append sb "&lt;")
            \> (.append sb "&gt;")
            (.append sb c))))
      (.append sb "</td></tr>"))
    (.append sb base-fortune-post)
    (.toString sb)))

(defmethod ig/init-key :hello.handler/fortunes [_ {:keys [db]}]
  (fn [{[_] :ataraxy/result}]
    (let [fortunes (prepare-fortunes (fortune-db/get-all db))]
      [::response/ok (fortunes-str fortunes)])))

