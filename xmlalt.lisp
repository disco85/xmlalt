(in-package :xmlalt)




(defun main ()
  (let ((parsed (xmlfmt::parse-xml #P"./t1.xml")))
    ;; (inspect parsed)
    (cmdfmt:serialize (cdr parsed) t)
    ))
