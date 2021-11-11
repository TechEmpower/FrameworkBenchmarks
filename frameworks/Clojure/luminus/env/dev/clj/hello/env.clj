(ns hello.env
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [hello.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[hello started successfully using the development profile]=-"))
   :middleware wrap-dev})
