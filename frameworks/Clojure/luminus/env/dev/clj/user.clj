(ns user
  (:require [mount.core :as mount]
            hello.core))

(defn start []
  (mount/start-without #'hello.core/repl-server))

(defn stop []
  (mount/stop-except #'hello.core/repl-server))

(defn restart []
  (stop)
  (start))


