(defun parse-argv (args)
  (flet ((parse-int-value (option value)
           (handler-case (parse-integer value)
             (error (e)
               (error "Invalid value for ~S: ~S~%  ~A" option value e)))))
    (loop for option = (pop args)
          for value = (pop args)
          while option
          if (not (starts-with option "--"))
            do (error "Invalid option: ~S" option)
          else
            if (equal option "--address")
              append (list :address value)
          else
            if (equal option "--port")
              append (list :port (parse-int-value option value))
          else
            if (equal option "--worker")
              append (list :worker-num (parse-int-value option value))
          else
            do (error "Unknown option: ~S" option))))
