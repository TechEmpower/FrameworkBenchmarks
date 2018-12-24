(defun starts-with (x starts)
  (and (<= (length starts) (length x))
       (string= x starts :end1 (length starts))))
