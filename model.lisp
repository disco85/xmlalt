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


(defclass text (node)
  ((open-by :initform "" :reader text-open-by)
   (close-by :initform "" :reader text-close-by)
   (content :type string
            :initarg :content
            :documentation ""
            :accessor text-content)))


(defclass pinstr (node)
  ((open-by :initform "<?" :reader pinstr-open-by)
   (close-by :initform "?>" :reader pinstr-close-by)
   (target :type string
           :initform ""
           :initarg :target
           :accessor pinstr-target
           :documentation "")
   (content :type string
            :documentation ""
            :initarg :content
            :accessor pinstr-content))
  (:documentation "Processing instruction"))


(defclass cdata (node)
  ((open-by :initform "<![CDATA[" :reader cdata-open-by)
   (close-by :initform "]]>" :reader cdata-close-by)
   (content :type string
            :documentation ""
            :initform ""
            :initarg :content
            :accessor cdata-content)))


(defclass comment (node)
  ((open-by :initform "<!--")
   (close-by :initform "-->")
   (content :type string
            :initarg :content
            :documentation ""
            :accessor comment-content)))


(defclass empty (node)  ;; FIXME wtf is it?
  ((attributes :type list
               :initform nil
               :documentation ""
               :accessor empty-attributes)
   (name :type name
         :documentation ""
         :accessor empty-name)))


(defclass prefix-mappings ()
  ((items :type list
          :initform nil
          :initarg :items
          :accessor prefix-mappings-items
          :documentation "Pairs (list of cons)")))


(defun add-prefix-mappings (prefix-mappings &rest new-pairs)
  (setf (prefix-mappings-items prefix-mappings)
        (append new-pairs
                (prefix-mappings-items prefix-mappings))))


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
   (prefix-mappings :type list
                    :documentation ""
                    :initform nil
                    :initarg :prefix-mappings
                    :accessor elem-prefix-mappings)
   (attributes :type list
               :initform nil
               :initarg :attributes
               :documentation ""
               :accessor elem-attributes)
   (children :type list
             :documentation ""
             :initform nil
             :accessor elem-children)))


(defclass entity (node) ; TODO do I use it?
  ((open-by :initform "&" :reader entity-open-by)
   (close-by :initform ";" :reader entity-close-by)
   (entity-code :type string
                :documentation ""
                :accessor entity-entity-code)))


(defclass doctype ()
  ((content :type string
            :documentation ""
            :accessor doctype-content
            :initarg :content)))


(defclass xml-decl ()
  ((content :type string
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
         :accessor nota-decl-name
         :initform ""
         :initarg :name)
   (public-id :type string
              :documentation ""
              :accessor nota-decl-public-id
              :initform ""
              :initarg :public-id)
   (system-id :type string
              :documentation ""
              :accessor nota-decl-system-id
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
  ((xml-decl :accessor doc-xml-decl ;; TODO find a way to populate it
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
