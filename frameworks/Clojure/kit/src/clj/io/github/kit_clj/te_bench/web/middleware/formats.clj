(ns io.github.kit-clj.te-bench.web.middleware.formats
  (:require
    [muuntaja.core :as m]
    [muuntaja.format.json :as json-format]))

(def instance
  (m/create (assoc m/default-options
              :formats {"application/json" json-format/format})))
