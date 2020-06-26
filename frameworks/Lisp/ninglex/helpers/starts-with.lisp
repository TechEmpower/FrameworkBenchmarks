(defun starts-with (search-in search-for)
  "Determine whether `str` starts with `search-for`"
  (declare (string search-in)
           (string search-for))
  (and (<= (length search-for) (length search-in))
       (string= search-in search-for :end1 (length search-for))))
