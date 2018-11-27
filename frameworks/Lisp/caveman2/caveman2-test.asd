(defsystem "caveman2-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Julian Coleman"
  :license ""
  :depends-on ("caveman2"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "caveman2"))))
  :description "Test system for caveman2"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
