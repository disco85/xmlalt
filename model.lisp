(in-package :model)

(defclass uri ()
  ((value :type string
          :documentation ""
          :accessor uri-value)))


(defclass ns ()
  ((uri :type uri
        :documentation ""
        :accessor ns-uri)))


(defclass prefix ()
  ((ns :initform nil :type ns
       :documentation ""
       :accessor prefix-ns)))


(defclass local-name ()
  ((value :type string
          :documentation ""
          :accessor local-name-value)))


(defclass name ()
  ((local-name :type local-name
               :documentation ""
               :accessor name-local-name)
   (prefix :initform nil
           :type prefix
           :documentation ""
           :accessor name-prefix)))


(defclass dir ()
  ((names :type cons
          :documentation "Non-empty list of NAME"
          :accessor dir-names)))

(defun path-depth (dir)
  (length (dir-names dir)))


(defclass content ()
  ((value :type string
          :initform ""
          :documentation ""
          :accessor content-value)))


(defclass node ()
  ((dir :type dir
        :documentation "A construct is located at some DIR reflecting it nesting"
        :accessor node-dir)
   (children :initform nil
             :type list
             :documentation "Refers zero or more children of its own type CONSTRUCT"
             :accessor node-children)
   (open-by :type string
            :initform "<"
            :reader node-open-by
            :documentation "")
   (close-by :type string
             :initform ">"
             :reader node-open-by
             :documentation "")))


(defclass attribute ()
  ((name :type name
         :documentation ""
         :accessor attribute-name)
   (value :type content
          :documentation ""
          :accessor attribute-value)))


(defclass text (node)
  ((open-by :initform "")
   (close-by :initform "")
   (conent :type content
           :documentation ""
           :accessor text-conent)))


(defclass pinstr (node)
  ((open-by :initform "<?")
   (close-by :initform "?>")
   (content :type content
            :documentation ""
            :accessor pinstr-content))
  (:documentation "Processing instruction"))


(defclass cdata (node)
  ((open-by :initform "<![CDATA[")
   (close-by :initform "]]>")
   (content :type content
            :initform nil
            :documentation ""
            :accessor cdata-content)))


(defclass comment (node)
  ((open-by :initform "<!--")
   (close-by :initform "-->")
   (content :type content
            :initform nil
            :documentation ""
            :accessor comment-content)))


(defclass empty (node)
  ((attributes :type list
               :initform nil
               :documentation ""
               :accessor empty-attributes)
   (name :type name
         :documentation ""
         :accessor empty-name)))


(defclass tag (node)
  ((attributes :type list
               :initform nil
               :documentation ""
               :accessor tag-attributes)
   (name :type name
         :documentation ""
         :accessor tag-name)))


(defclass entity (node)
  ((open-by :initform "&")
   (close-by :initform ";")
   (entity-code :type content
                :documentation ""
                :accessor entity-entity-code)))


(defclass doctype ()
  ((content :type content
            :documentation ""
            :accessor doctype-content
            :initarg :content)))


(defclass xml-decl ()
  ((content :type content
            :documentation ""
            :accessor xml-decl-content
            :initarg :content)))


(defclass doc ()
  ((xml-decl :accessor doc-xml-decl
             :initform nil
             :initarg :xml-decl)
   (doctype )))
