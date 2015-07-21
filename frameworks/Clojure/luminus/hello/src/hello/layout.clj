(ns hello.layout
  (:require [selmer.parser :as parser]
            [selmer.filters :as filters]
            [markdown.core :refer [md-to-html-string]]
            [ring.util.response :refer [content-type response]]
            [compojure.response :refer [Renderable]]
            [ring.util.anti-forgery :refer [anti-forgery-field]]
            [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
            [environ.core :refer [env]]))

(parser/set-resource-path!  (clojure.java.io/resource "templates"))

(parser/add-tag! :csrf-field (fn [_ _] (anti-forgery-field)))
(filters/add-filter! :markdown (fn [content] [:safe (md-to-html-string content)]))

(deftype RenderableTemplate [template params]
  Renderable
  (render [this request]
    (content-type
      (->> (assoc params
                  :page template
                  :dev (env :dev)
                  :csrf-token *anti-forgery-token*
                  :servlet-context
                  (if-let [context (:servlet-context request)]
                    ;; If we're not inside a serlvet environment (for
                    ;; example when using mock requests), then
                    ;; .getContextPath might not exist
                    (try (.getContextPath context)
                         (catch IllegalArgumentException _ context))))
        (parser/render-file (str template))
        response)
      "text/html; charset=utf-8")))

(defn render [template & [params]]
  (RenderableTemplate. template params))

