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


(defclass dir ()
  ((names :type cons
          :documentation "Non-empty list of NAME")))

(defun path-depth (path)
  (length (path-names)))


(defclass content ()
  ((value :type string :initform nil)))


(defclass node ()
  ((dir :type dir
         :documentation "A construct is located at some DIR reflecting it nesting")
   (children :initform nil :type list
             :documentation "Refers zero or more children of its own type CONSTRUCT")
   (open-by :type string :initform "<" :reader node-open-by)
   (close-by :type string :initform ">" :reader node-open-by)))


(defclass attribute ()
  ((name :type name)
   (value :type content)))


(defclass text (node)
  ((open-by :initform "")
   (close-by :initform "")
   (conent :type content)))


(defclass pi (node)
  ((open-by :initform "<?")
   (close-by :initform "?>")
   (content :type content)))


(defclass cdata (node)
  ((open-by :initform "<![CDATA[")
   (close-by :initform "]]>")
   (content :type content :initform nil)))


(defclass comment (node)
  ((open-by :initform "<!--")
   (close-by :initform "-->")
   (content :type content :initform nil)))


(defclass empty (node)
  ((attributes :type list :initform :nil)
   (name :type name)))


(defclass tag (node)
  ((attributes :type list :initform nil)
   (name :type name)))


(defclass entity (node)
  ((open-by :initform "&")
   (close-by :initform ";")
   (entity-code :type content)))


(defclass doctype ()
  ((content :type content)))


(defclass xml-decl ()
  ((content :type content)))

;; TODO
