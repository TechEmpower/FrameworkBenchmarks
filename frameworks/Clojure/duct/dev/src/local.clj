;; Local REPL configuration

;; Override read-config
(defn read-config []
  (duct/read-config (io/resource "local.edn")))
