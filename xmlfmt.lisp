(in-package :xmlfmt)


(defclass mysax (sax:abstract-handler)
  ((doc :initform '(make-instance model:doc) :accessor doc)))

(defmethod sax:attribute-declaration ((handler mysax) element-name attribute-name type default)
  (format t "ATTRIBUTE-DECLARATION! ELEMENT-NAME: ~A ATTRIBUTE-NAME: ~A TYPE: ~A DEFAULT: ~A~%~%"
          element-name attribute-name type default))

(defmethod sax:start-document ((handler mysax))
  (format t "START-DOCUMENT!~%~%"))

(defmethod sax:start-dtd ((handler mysax) name public-id system-id)
  (format t "START-DTD! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%" name public-id system-id))

(defmethod sax::dtd ((handler mysax) dtd)  ;; dtd is internal and must be defined
  (format t "DTD! DTD: ~A~%~%" dtd))

(defmethod sax:start-internal-subset ((handler mysax))
  (format t "START-INTERNAL-SUBSET!~%~%"))

(defmethod sax:element-declaration ((handler mysax) name model)
  (format t "ELEMENT-DECLARATION! NAME: ~A MODEL: ~A~%~%" name model))

(defmethod sax:attribute-declaration ((handler mysax) element-name attribute-name type default)
  (format t "ATTRIBUTE-DECLARATION! ELEMENT-NAME: ~A ATTRIBUTE-NAME: ~A TYPE: ~A DEFAULT: ~A~%~%"
          element-name attribute-name type default))

(defmethod sax:notation-declaration ((handler mysax) name public-id system-id)
  (format t "NOTATION-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
          name public-id system-id))

(defmethod sax:internal-entity-declaration ((handler mysax) kind name value)
  (format t "INTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A VALUE: ~A~%~%" kind name value))

(defmethod sax:external-entity-declaration ((handler mysax) kind name public-id system-id)
  (format t "EXTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
          kind name public-id system-id))

(defmethod sax:unparsed-entity-declaration ((handler mysax) name public-id system-id notation-name)
  (format t "UNPARSED-ENTITY-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A NOTATION-NAME: ~A~%~%"
          name public-id system-id notation-name))

(defmethod sax:unparsed-internal-subset ((handler mysax) str)
  (format t "UNPARSED-INTERNAL-SUBSET! STR: ~A~%~%" str))

(defmethod sax:end-internal-subset ((handler mysax))
  (format t "END-INTERNAL-SUBSET!~%~%"))

(defmethod sax:end-dtd ((handler mysax))
  (format t "END-DTD!~%~%"))

(defmethod sax:start-prefix-mapping ((handler mysax) prefix uri)
  (format t "START-PREFIX-MAPPING! PREFIX: ~A URI: ~A~%~%" prefix uri))

(defmethod sax:start-element ((handler mysax) namespace-uri local-name qname attributes)
  (format t "START-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A ATTRIBUTES: ~A~%~%"
          namespace-uri local-name qname attributes))

(defmethod sax:comment ((handler mysax) data)
  (format t "COMMENT! DATA: ~A~%~%" data))

(defmethod sax:start-cdata ((handler mysax))
  (format t "START-CDATA!~%~%"))

(defmethod sax:characters ((handler mysax) data)
  (format t "CHARACTERS! DATA: ~A~%~%" data))

(defmethod sax:end-cdata ((handler mysax))
  (format t "END-CDATA!~%~%"))

(defmethod sax:processing-instruction ((handler mysax) target data)
  (format t "PROCESSING-INSTRUCTION! TARGET: ~A DATA: ~A~%~%" target data))

(defmethod sax:end-element ((handler mysax) namespace-uri local-name qname)
  (format t "END-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A~%~%"
          namespace-uri local-name qname))

(defmethod sax:end-prefix-mapping ((handler mysax) prefix)
  (format t "END-PREFIX-MAPPING! PREFIX: ~A~%~%" prefix))

(defmethod sax:end-document ((handler mysax))
  (format t "END-DOCUMENT!~%~%"))

(defmethod sax:entity-resolver ((handler mysax) resolver)
  (format t "ENTITY-RESOLVER! RESOLVER: ~A~%~%" resolver))

(defmethod sax:unescaped ((handler mysax) data)
  (format t "UNESCAPED! DATA: ~A~%~%" data))


(defun parse-xml (path)
  (let ((mysax (make-instance 'mysax)))
    (handler-case
        (progn (call-with-input-stream
                path
                (lambda (f) (cxml:parse f mysax)))
               (cons :ok (mysax-doc mysax)))
      (error (x) (cons :fail (format nil "Parsing of XML '~A' failed: ~A" path x))))))
