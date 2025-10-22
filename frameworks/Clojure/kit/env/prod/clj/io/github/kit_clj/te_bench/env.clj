(ns io.github.kit-clj.te-bench.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init       (fn []
                 (log/info "\n-=[ starting]=-"))
   :start      (fn []
                 (log/info "\n-=[ started successfully]=-"))
   :stop       (fn []
                 (log/info "\n-=[ has shut down successfully]=-"))
   :opts       {:profile :prod}})
