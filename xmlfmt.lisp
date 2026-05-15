(in-package :xmlfmt)


(defclass mysax (sax:abstract-handler)
  ((doc :initform (make-instance 'model:doc) :accessor mysax-doc)))

(defmethod sax:attribute-declaration ((mysax mysax) element-name attribute-name type default)
  (let ((attr-decl (make-instance 'model:attr-decl :element-name element-name
                                                   :attribute-name attribute-name
                                                   :type type
                                                   :default default)))
    (push attr-decl (model:dtd-items (model:doc-dtd (mysax-doc mysax))))
    (format t "ATTRIBUTE-DECLARATION! ELEMENT-NAME: ~A ATTRIBUTE-NAME: ~A TYPE: ~A DEFAULT: ~A~%~%"
            element-name attribute-name type default)))

(defmethod sax:start-document ((mysax mysax))
  (format t "START-DOCUMENT!~%~%"))

(defmethod sax:start-dtd ((mysax mysax) name public-id system-id)
  (setf (model:doc-dtd (mysax-doc mysax))
        (make-instance 'model:dtd :name name :public-id public-id :system-id system-id))
  (format t "START-DTD! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%" name public-id system-id))

(defmethod sax::dtd ((mysax mysax) dtd)  ;; dtd is internal and must be defined
  (format t "DTD! DTD: ~A~%~%" dtd))

(defmethod sax:start-internal-subset ((mysax mysax))
  (format t "START-INTERNAL-SUBSET!~%~%"))

(defmethod sax:element-declaration ((mysax mysax) name model)
  (let ((elem-decl (make-instance 'model:elem-decl :name name
                                                   :model model)))
    (push elem-decl (model:dtd-items (model:doc-dtd (mysax-doc mysax))))
    (format t "ELEMENT-DECLARATION! NAME: ~A MODEL: ~A~%~%" name model)))

(defmethod sax:notation-declaration ((mysax mysax) name public-id system-id)
  (let ((nota-decl (make-instance 'model:nota-decl :name name
                                                   :public-id public-id
                                                   :system-id system-id)))
    (push nota-decl (model:dtd-items (model:doc-dtd (mysax-doc mysax))))
    (format t "NOTATION-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
            name public-id system-id)))

(defmethod sax:internal-entity-declaration ((mysax mysax) kind name value)
  (let ((int-ent-decl (make-instance 'model:int-ent-decl :kind kind
                                                         :name name
                                                         :value value)))
    (push int-ent-decl (model:dtd-items (model:doc-dtd (mysax-doc mysax))))
    (format t "INTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A VALUE: ~A~%~%" kind name value)))

(defmethod sax:external-entity-declaration ((mysax mysax) kind name public-id system-id)
  (let ((ext-ent-decl (make-instance 'model:ext-ent-decl :kind kind
                                                         :name name
                                                         :public-id public-id
                                                         :system-id system-id)))
    (push ext-ent-decl (model:dtd-items (model:doc-dtd (mysax-doc mysax))))
    (format t "EXTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
            kind name public-id system-id)))

(defmethod sax:unparsed-entity-declaration ((mysax mysax) name public-id system-id notation-name)
  (let ((unp-ent-decl (make-instance 'model:unp-ent-decl :name name
                                                         :public-id public-id
                                                         :system-id system-id
                                                         :nota-name notation-name)))
    (push unp-ent-decl (model:dtd-items (model:doc-dtd (mysax-doc mysax))))
    (format t "UNPARSED-ENTITY-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A NOTATION-NAME: ~A~%~%"
            name public-id system-id notation-name)))

(defmethod sax:unparsed-internal-subset ((mysax mysax) str)
  (let ((unp-int-subs (make-instance 'model:unp-int-subs :content cont)))
    (push unp-int-subs (model:dtd-items (model:doc-dtd (mysax-doc mysax))))
  (format t "UNPARSED-INTERNAL-SUBSET! STR: ~A~%~%" str)))

(defmethod sax:end-internal-subset ((mysax mysax))
  (format t "END-INTERNAL-SUBSET!~%~%"))

(defmethod sax:end-dtd ((mysax mysax))
  (format t "END-DTD!~%~%"))

(defmethod sax:start-prefix-mapping ((mysax mysax) prefix uri)
  (format t "START-PREFIX-MAPPING! PREFIX: ~A URI: ~A~%~%" prefix uri))

(defmethod sax:start-element ((mysax mysax) namespace-uri local-name qname attributes)
  (let ((elem (make-instance 'model:elem :namespace-uri namespace-uri
                                         :local-name local-name
                                         :qname qname
                                         :attributes attributes)))
    (push elem (model:doc-elems-stack (mysax-doc mysax)))
    (format t "START-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A ATTRIBUTES: ~A~%~%"
            namespace-uri local-name qname attributes)))

(defmethod sax:comment ((mysax mysax) data)
  (format t "COMMENT! DATA: ~A~%~%" data))

(defmethod sax:start-cdata ((mysax mysax))
  (format t "START-CDATA!~%~%"))

(defmethod sax:characters ((mysax mysax) data)
  (format t "CHARACTERS! DATA: ~A~%~%" data))

(defmethod sax:end-cdata ((mysax mysax))
  (format t "END-CDATA!~%~%"))

(defmethod sax:processing-instruction ((mysax mysax) target data)
  (format t "PROCESSING-INSTRUCTION! TARGET: ~A DATA: ~A~%~%" target data))

(defmethod sax:end-element ((mysax mysax) namespace-uri local-name qname)
  (symbol-macrolet ((elems-stack (model:doc-elems-stack (mysax-doc mysax))))
    (when (cdr elems-stack)
      (pop elems-stack)))
  (format t "END-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A~%~%"
          namespace-uri local-name qname))

(defmethod sax:end-prefix-mapping ((mysax mysax) prefix)
  (format t "END-PREFIX-MAPPING! PREFIX: ~A~%~%" prefix))

(defmethod sax:end-document ((mysax mysax))
  (format t "END-DOCUMENT!~%~%"))

(defmethod sax:entity-resolver ((mysax mysax) resolver)
  (format t "ENTITY-RESOLVER! RESOLVER: ~A~%~%" resolver))

(defmethod sax:unescaped ((mysax mysax) data)
  (format t "UNESCAPED! DATA: ~A~%~%" data))


(defun parse-xml (path)
  (let ((mysax (make-instance 'mysax)))
    (handler-case
        (progn (utils:call-with-input-stream
                path
                (lambda (f) (cxml:parse f mysax)))
               (cons :ok (mysax-doc mysax)))
      (error (x) (cons :fail (format nil "Parsing of XML '~A' failed: ~A" path x))))))
