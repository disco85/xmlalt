(in-package :xmlfmt)

;; (defclass event-collecting-handler ()
;;   ((event-list :initform '() :accessor event-list)))

;; (defmethod start-document ((handler event-collecting-handler))
;;   (push (list :start-document) (event-list handler)))

;; (defmethod start-element ((handler event-collecting-handler) ns-uri local-name qname attrs)
;;   (push (list :start-element ns-uri local-name qname attrs)
;; 	(event-list handler)))

;; (defmethod start-prefix-mapping ((handler event-collecting-handler) prefix uri)
;;   (push (list :start-prefix-mapping prefix uri)
;; 	(event-list handler)))

;; (defmethod characters ((handler event-collecting-handler) data)
;;   (push (list :characters data)
;; 	(event-list handler)))

;; (defmethod processing-instruction ((handler event-collecting-handler) target data)
;;   (push (list :processing-instruction target data)
;; 	(event-list handler)))

;; (defmethod end-prefix-mapping ((handler event-collecting-handler) prefix)
;;   (push (list :end-prefix-mapping prefix)
;; 	(event-list handler)))

;; (defmethod end-element ((handler event-collecting-handler) namespace-uri local-name qname)
;;   (push (list :end-element namespace-uri local-name qname)
;; 	(event-list handler)))

;; (defmethod end-document ((handler event-collecting-handler))
;;   (push (list :end-document)
;; 	(event-list handler))
;;   (nreverse (event-list handler)))



(defclass myclass (sax:abstract-handler)
  ((event-list :initform '() :accessor event-list)))

(defmethod sax:attribute-declaration ((handler myclass) element-name attribute-name type default)
  (format t "ATTRIBUTE-DECLARATION! ELEMENT-NAME: ~A ATTRIBUTE-NAME: ~A TYPE: ~A DEFAULT: ~A~%~%"
          element-name attribute-name type default))

(defmethod sax:start-document ((handler myclass))
  (format t "START-DOCUMENT!~%~%"))

(defmethod sax:start-dtd ((handler myclass) name public-id system-id)
  (format t "START-DTD! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%" name public-id system-id))

(defmethod sax::dtd ((handler myclass) dtd)
  (format t "START-DTD! DTD: ~A~%~%" dtd))

(defmethod sax:start-internal-subset ((handler myclass))
  (format t "START-INTERNAL-SUBSET!~%~%"))

(defmethod sax:element-declaration ((handler myclass) name model)
  (format t "ELEMENT-DECLARATION! NAME: ~A MODEL: ~A~%~%" name model))

(defmethod sax:attribute-declaration ((handler myclass) element-name attribute-name type default)
  (format t "ATTRIBUTE-DECLARATION! ELEMENT-NAME: ~A ATTRIBUTE-NAME: ~A TYPE: ~A DEFAULT: ~A~%~%"
          element-name attribute-name type default))

(defmethod sax:notation-declaration ((handler myclass) name public-id system-id)
  (format t "NOTATION-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
          name public-id system-id))

(defmethod sax:internal-entity-declaration ((handler myclass) kind name value)
  (format t "INTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A VALUE: ~A~%~%" kind name value))

(defmethod sax:external-entity-declaration ((handler myclass) kind name public-id system-id)
  (format t "EXTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
          kind name public-id system-id))

(defmethod sax:unparsed-entity-declaration ((handler myclass) name public-id system-id notation-name)
  (format t "UNPARSED-ENTITY-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A NOTATION-NAME: ~A~%~%"
          name public-id system-id notation-name))

(defmethod sax:unparsed-internal-subset ((handler myclass) str)
  (format t "UNPARSED-INTERNAL-SUBSET! STR: ~A~%~%" str))

(defmethod sax:end-internal-subset ((handler myclass))
  (format t "END-INTERNAL-SUBSET!~%~%"))

(defmethod sax:end-dtd ((handler myclass))
  (format t "END-DTD!~%~%"))

(defmethod sax:start-prefix-mapping ((handler myclass) prefix uri)
  (format t "START-PREFIX-MAPPING! PREFIX: ~A URI: ~A~%~%" prefix uri))

(defmethod sax:start-element ((handler myclass) namespace-uri local-name qname attributes)
  (format t "START-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A ATTRIBUTES: ~A~%~%"
          namespace-uri local-name qname attributes))

(defmethod sax:comment ((handler myclass) data)
  (format t "COMMENT! DATA: ~A~%~%" data))

(defmethod sax:start-cdata ((handler myclass))
  (format t "START-CDATA!~%~%"))

(defmethod sax:characters ((handler myclass) data)
  (format t "CHARACTERS! DATA: ~A~%~%" data))

(defmethod sax:end-cdata ((handler myclass))
  (format t "END-CDATA!~%~%"))

(defmethod sax:processing-instruction ((handler myclass) target data)
  (format t "PROCESSING-INSTRUCTION! TARGET: ~A DATA: ~A~%~%" target data))

(defmethod sax:end-element ((handler myclass) namespace-uri local-name qname)
  (format t "END-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A~%~%"
          namespace-uri local-name qname))

(defmethod sax:end-prefix-mapping ((handler myclass) prefix)
  (format t "END-PREFIX-MAPPING! PREFIX: ~A~%~%" prefix))

(defmethod sax:end-document ((handler myclass))
  (format t "END-DOCUMENT!~%~%"))

(defmethod sax:entity-resolver ((handler myclass) resolver)
  (format t "ENTITY-RESOLVER! RESOLVER: ~A~%~%" resolver))

(defmethod sax:unescaped ((handler myclass) data)
  (format t "UNESCAPED! DATA: ~A~%~%" data))


(defun parse-xml (path)
  (let ((handler (make-instance 'myclass)))
    (cxml:parse path handler)))

  ;; (with-open-file (in path :direction :input)
  ;;   (let ((handler (make-instance 'myclass))
  ;;         (buf (read in)))
  ;;     (cxml:parse buf handler))))
