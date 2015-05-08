(ns hello.session
  (:require [cronj.core :refer [cronj]]))

(defonce mem (atom {}))

(defn- current-time []
  (quot (System/currentTimeMillis) 1000))

(defn- expired? [[id session]]
  (pos? (- (:ring.middleware.session-timeout/idle-timeout session) (current-time))))

(defn clear-expired-sessions []
  (clojure.core/swap! mem #(->> % (filter expired?) (into {}))))

(def cleanup-job
  (cronj
    :entries
    [{:id "session-cleanup"
      :handler (fn [_ _] (clear-expired-sessions))
      :schedule "* /30 * * * * *"
      :opts {}}]))
