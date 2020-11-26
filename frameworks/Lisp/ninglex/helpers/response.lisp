(defun merge-plist (p1 p2)
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound)
        do (progn
             (push value p2)
             (push indicator p2)))
  p2)

(defun html-response* (response &optional headers)
  "This hould be a docstring"
  `(
     200
     ,(merge-plist '(:content-type "text/html; charset=utf-8" :server "Woo") headers)
     (,response)
   ))

(defun json-response* (response &optional headers)
  "This hould be a docstring"
  `(
     200
     ,(merge-plist '(:content-type "application/json; charset=utf-8" :server "Woo") headers)
     (,response)
   ))

(defun plain-response* (response &optional headers)
  "This hould be a docstring"
  `(
     200
     ,(merge-plist '(:content-type "text/plain; charset=utf-8" :server "Woo") headers)
     (,response)
   ))
