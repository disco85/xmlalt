(in-package :xmlalt)




(defun main ()
  (let ((parsed (xmlfmt::parse-xml #P"./t1.xml")))
    (format t "~A~%" parsed)))
