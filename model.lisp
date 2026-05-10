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
  ((open-by :initform "" :reader text-open-by)
   (close-by :initform "" :reader text-close-by)
   (conent :type content
           :documentation ""
           :accessor text-conent)))


(defclass pinstr (node)
  ((open-by :initform "<?" :reader pinstr-open-by)
   (close-by :initform "?>" :reader pinstr-close-by)
   (content :type content
            :documentation ""
            :accessor pinstr-content))
  (:documentation "Processing instruction"))


(defclass cdata (node)
  ((open-by :initform "<![CDATA[" :reader cdata-open-by)
   (close-by :initform "]]>" :reader cdata-close-by)
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


(defclass elem (node)
  ((attributes :type list
               :initform nil
               :documentation ""
               :accessor elem-attributes)
   (name :type name
         :documentation ""
         :accessor elem-name)))


(defclass entity (node)
  ((open-by :initform "&" :reader entity-open-by)
   (close-by :initform ";" :reader entity-close-by)
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

(defclass elem-decl ()
  ((content :type content
            :documentation ""
            :accessor elem-decl-content
            :initarg :content)))

(defclass attr-decl ()
  ((content :type content
            :documentation ""
            :accessor attr-decl-content
            :initarg :content)))

(defclass not-decl ()
  ((content :type content
            :documentation ""
            :accessor not-decl-content
            :initarg :content)))

(deftype entity-decl-kind-type ()
  '(member :internal :external :unparsed))

(defclass entity-decl ()
  ((content :type content
            :documentation ""
            :accessor entity-decl-content
            :initarg :content)
   (kind :type entity-decl-kind-type
         :documentation ""
         :accessor entity-decl-kind
         :initarg :kind)))

(defclass dtd ()
  ((items :type list  ;; items as attr-decl, elem-decl...
          :documentation ""
          :accessor dtd-items
          :initform nil
          :initarg :items)))

(defclass doc ()
  ((xml-decl :accessor doc-xml-decl
             :initform nil
             :initarg :xml-decl)
   (dtd :accessor dtd
        :documentation ""
        :initform nil)))
