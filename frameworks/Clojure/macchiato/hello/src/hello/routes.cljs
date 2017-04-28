(ns hello.routes
  (:require
    [bidi.bidi :as bidi]
    [clojure.string :as st]
    [hello.db :as db]
    [hiccups.runtime]
    [macchiato.util.response :as r])
  (:require-macros
    [hiccups.core :refer [html]]))


(defn not-found [req res raise]
  (-> (html
        [:html
         [:body
          [:h2 (:uri req) " was not found"]]])
      (r/not-found)
      (r/content-type "text/html")
      (res)))

(defn json-serialization
  "Test 1: JSON serialization"
  [_ res _]
  (-> (js/JSON.stringify #js {:message "Hello, World!"})
      (r/ok)
      (r/content-type "application/json")
      (res)))

(defn plaintext [_ res _]
  (-> (r/ok "Hello, World!")
      (r/content-type "text/html")
      (res)))

(defn single-query-test [req res raise]
  (db/run-query
    #(-> (js/JSON.stringify %)
         (r/ok)
         (r/content-type "application/json")
         (res))
    raise))

(defn escape-html [s]
  (st/escape s
             {"&"  "&amp;"
              ">"  "&gt;"
              "<"  "&lt;"
              "\"" "&quot;"}))

(defn fortunes-test [req res raise]
  (db/get-fortunes
    #(-> (str
           "<!doctype html>"
           (html
             [:html
              [:head [:title "Fortunes"]]
              [:body
               [:table
                [:tr [:th "id"] [:th "message"]]
                (for [message %]
                  [:tr [:td (:id message)] [:td (-> message :message escape-html)]])]]]))
         (r/ok)
         (r/content-type "text/html;charset=utf-8")
         (res))
    raise))

(defn queries-test [req res raise]
  (db/run-queries
    (or (-> req :route-params :queries) 1)
    #(-> (js/JSON.stringify %)
         (r/ok)
         (r/content-type "application/json")
         (res))
    raise))

(defn update-test [req res raise]
  (db/update-and-persist
    (or (-> req :route-params :queries) 1)
    #(-> (js/JSON.stringify %)
         (r/ok)
         (r/content-type "application/json")
         (res))
    raise))

(def routes
  ["/" {""          {:get (fn [_ res _] (res (r/ok "Hello, World!")))}
        "plaintext" {:get plaintext}
        "json"      {:get json-serialization}
        "db"        {:get single-query-test}
        "fortunes"  {:get fortunes-test}
        "queries"
                    {"/"            {:get queries-test}
                     ["/" :queries] {:get queries-test}}
        "updates"   {"/"            {:get update-test}
                     ["/" :queries] {:get update-test}}}])

(defn router [req res raise]
  (if-let [{:keys [handler route-params]} (bidi/match-route* routes (:uri req) req)]
    (handler (assoc req :route-params route-params) res raise)
    (not-found req res raise)))
