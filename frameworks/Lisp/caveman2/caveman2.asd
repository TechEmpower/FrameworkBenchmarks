(defsystem "caveman2"
  :version "0.1.0"
  :author "Julian Coleman"
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "caveman2-test"))))
