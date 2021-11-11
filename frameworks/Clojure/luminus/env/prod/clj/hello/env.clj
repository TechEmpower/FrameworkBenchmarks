(ns hello.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[hello started successfully]=-"))
   :middleware identity})
