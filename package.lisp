(defpackage :xmlalt
  (:use :cl)
  (:export :main))

(defpackage :model
  (:use :cl)
  (:export :main
           :uri))

(defpackage :xmlalt-tests
  (:use :cl :xmlalt)
  )
