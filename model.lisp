(in-package :model)

(defclass uri ()  ;; TODO use URI, NS, PREFIX
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
   (open-by :type string
            :initform "<"
            :reader node-open-by
            :documentation "")
   (close-by :type string
             :initform ">"
             :reader node-open-by
             :documentation "")))


(defclass attr ()
  ((namespace-uri :type string
                  :documentation ""
                  :initform ""
                  :initarg :namespace-uri
                  :accessor attr-namespace-uri)
   (local-name :type string
               :documentation ""
               :initform ""
               :initarg :local-name
               :accessor attr-local-name)
   (qname :type string  ;; TODO start to use my NAME class
          :documentation ""
          :initform ""
          :initarg :qname
          :accessor attr-qname)
   (value :type string
          :documentation ""
          :initform ""
          :initarg :value
          :accessor attr-value)
   (specified :type boolean
              :documentation ""
              :initform nil
              :initarg :specified
              :accessor attr-specified)))
;; #S(SAX::STANDARD-ATTRIBUTE
;;                    :NAMESPACE-URI "http://www.w3.org/2000/xmlns/"
;;                    :LOCAL-NAME NIL
;;                    :QNAME "xmlns"
;;                    :VALUE "urn:default"
;;                    :SPECIFIED-P T)

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
  ((namespace-uri :type string
                  :initform ""
                  :initarg :namespace-uri
                  :documentation ""
                  :accessor elem-namespace-uri)
   (local-name :type string
               :initform ""
               :initarg :local-name
               :documentation ""
               :accessor elem-local-name)
   (qname :type string
          :initform ""
          :initarg :qname
          :documentation ""
          :accessor elem-qname)
   (attributes :type list
               :initform nil
               :initarg :attributes
               :documentation ""
               :accessor elem-attributes)
   (children :type list
             :documentation ""
             :initform nil
             :accessor elem-children)))


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
  ((name :type string
         :documentation ""
         :accessor elem-decl-name
         :initarg :name)
   (model :type string
          :documentation ""
          :accessor elem-decl-model
          :initarg :model)))

(defclass attr-decl ()
  ((element-name :type string
                 :documentation ""
                 :accessor attr-decl-element-name
                 :initarg :element-name)
   (attribute-name :type string
                   :documentation ""
                   :accessor attr-decl-attribute-name
                   :initarg :attribute-name)
   (type :type string
         :documentation ""
         :accessor attr-decl-type
         :initarg :type)
   (default :type string
            :documentation ""
            :accessor attr-decl-default
            :initarg :default)))

(defclass nota-decl ()
  ((name :type string
         :documentation ""
         :accessor nota-name
         :initform ""
         :initarg :name)
   (public-id :type string
              :documentation ""
              :accessor nota-public-id
              :initform ""
              :initarg :public-id)
   (system-id :type string
              :documentation ""
              :accessor nota-system-id
              :initform ""
              :initarg :system-id)))

(defclass int-ent-decl ()
  ((kind :type string
         :documentation ""
         :accessor int-ent-decl-kind
         :initform ""
         :initarg :kind)
   (name :type string
         :documentation ""
         :accessor int-ent-decl-name
         :initform ""
         :initarg :name)
   (value :type string
          :documentation ""
          :accessor int-ent-decl-value
          :initform ""
          :initarg :value)))

(defclass ext-ent-decl ()
  ((kind :type string
         :documentation ""
         :accessor ext-ent-decl-kind
         :initform ""
         :initarg :kind)
   (name :type string
         :documentation ""
         :accessor ext-ent-decl-name
         :initform ""
         :initarg :name)
   (public-id :type string
              :documentation ""
              :accessor ext-ent-decl-public-id
              :initform ""
              :initarg :public-id)
   (system-id :type string
              :documentation ""
              :accessor ext-ent-decl-system-id
              :initform ""
              :initarg :system-id)))

(defclass unp-ent-decl ()
  ((name :type string
         :documentation ""
         :accessor unp-ent-decl-name
         :initform ""
         :initarg :name)
   (public-id :type string
              :documentation ""
              :accessor unp-ent-decl-public-id
              :initform ""
              :initarg :public-id)
   (system-id :type string
              :documentation ""
              :accessor unp-ent-decl-system-id
              :initform ""
              :initarg :system-id)
   (nota-name :type string
              :documentation ""
              :accessor unp-ent-decl-nota-name
              :initform ""
              :initarg :nota-name)))

(defclass unp-int-subs ()
  ((content :type string
         :documentation ""
         :accessor unp-int-subs-content
         :initform ""
         :initarg :content)))

(defclass dtd ()
  ((items :type list  ;; items as attr-decl, elem-decl...
          :documentation ""
          :accessor dtd-items
          :initform nil
          :initarg :items)
   (name :type string
         :documentation ""
         :accessor dtd-name
         :initform ""
         :initarg :name)
   (public-id :type string
              :documentation ""
              :accessor dtd-public-id
              :initform ""
              :initarg :public-id)
   (system-id :type string
              :documentation ""
              :accessor dtd-system-id
              :initform ""
              :initarg :system-id)))

(defclass doc ()
  ((xml-decl :accessor doc-xml-decl
             :initform nil
             :initarg :xml-decl)
   (dtd :type dtd
        :accessor doc-dtd
        :documentation ""
        :initform nil)
   (elems-stack :type list  ;; the last elem is the root
                :documentation ""
                :initform nil
                :accessor doc-elems-stack)))
