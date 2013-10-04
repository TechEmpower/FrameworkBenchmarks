(ns hello.views.layout
  (:use noir.request)
  (:require [selmer.parser :as parser]))

(def template-path "hello/views/templates/")

(defn render [template & [params]]
  (parser/render-file (str template-path template)
                      (assoc params :context (:context *request*))))

