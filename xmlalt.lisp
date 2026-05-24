(in-package :xmlalt)



(defun main ()
  (let* ((parsed (xmlfmt::parse-xml #P"./t1.xml"))
         (res (car parsed)))
    (if (eq res :ok)
        (progn (inspect parsed)
               (cmdfmt:serialize (cdr parsed) t)
               (format t "~%~%~%")
               (regfmt:serialize (cdr parsed) t))
        (format t "ERROR: ~A~%" (cdr parsed))
        )))
