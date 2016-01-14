(ns hello.config
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[hello started successfully]=-"))
   :middleware identity})
