(defpackage :xmlalt
  (:use :cl)
  (:export :main))

(defpackage :model
  (:use :cl)
  (:export :main
           :uri))

(defpackage :blkfmt
  (:use :cl))

(defpackage :cmdfmt
  (:use :cl))

(defpackage :mrkfmt
  (:use :cl))

(defpackage :regfmt
  (:use :cl))

(defpackage :xmlfmt
  (:use :cl))


(defpackage :xmlalt-tests
  (:use :cl :xmlalt)
  )
