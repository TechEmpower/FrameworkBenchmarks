(in-package :cl-user)
(defpackage caveman2.web
  (:use :cl
        :caveman2
        :caveman2.config
        :caveman2.view
        :caveman2.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :caveman2.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

@route GET "/plaintext"
(lambda () ("Hello, World!"))

@route GET "/json"
(lambda () (render-json (:|message| "Hello, World!")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
