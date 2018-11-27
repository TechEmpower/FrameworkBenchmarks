(ql:quickload :caveman2)

(defpackage caveman2.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :caveman2.web
                :*web*)
  (:import-from :caveman2.config
                :config
                :productionp
                :*static-directory*))
(in-package :caveman2.app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
