(in-package :model)

(defclass uri ()
  ((value :type string)))


(defclass ns ()
  ((uri :type uri)))


(defclass prefix ()
  ((ns :initform nil :type ns)))


(defclass local-name ()
  ((value :type string)))


(defclass name ()
  ((local-name :type local-name)
   (prefix :initform nil :type prefix)))


(defclass path ()
  ((names :type cons
          :documentation "Non-empty list of NAME")))

(defun path-depth (path)
  (length (path-names)))


(defclass construct ()
  ((path :type path
         :documentation "A construct is located at some PATH reflecting it nesting")
   (children :initform nil :type list
             :documentation "Refers children CONSTRUCT")))
