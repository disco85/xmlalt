;; TERMINOLOGY:
;;
;; XML construct ([cur-]xml-construct) - any construction/item inside XML
;; DTD, items in DTD, elements, comments

(in-package :xmlfmt)


(defclass mysax (sax:abstract-handler)
  ((doc :type model:doc
        :documentation ""
        :initform (model:create-doc)
        :accessor mysax-doc)
   (prefix-mappings :type list
                    :initform nil
                    :accessor mysax-prefix-mappings
                    :documentation "Current list of PREFIX-MAPPINGS. Their events are fired before their element,
so we save them first here, then add to an element, also they are scoped")
   (characters :type string
               :initform ""
               :accessor mysax-characters
               :documentation "Buffer to accumulate fragments of characters")))


(defun remember-prefix-mapping (mysax prefix uri)
  "Adds to current prefix mappings yet another mapping"
  (unless (mysax-prefix-mappings mysax)
    (push (model:create-prefix-mappings)
          (mysax-prefix-mappings mysax)))
  (model:add-prefix-mappings (car (mysax-prefix-mappings mysax))
                             (cons prefix uri)))


(defun forget-prefix-mappings (mysax)
  "Drops current prefix mappins"
  (pop (mysax-prefix-mappings mysax)))


(defmethod sax:attribute-declaration ((mysax mysax) element-name attribute-name type default)
  (let ((attr-decl (model:create-attr-decl :elem-name element-name
                                           :attr-name attribute-name
                                           :type type
                                           :default default)))
    (model:add-dtd-item (model:get-doc-dtd (mysax-doc mysax)) attr-decl)
    (format t "ATTRIBUTE-DECLARATION! ELEMENT-NAME: ~A ATTRIBUTE-NAME: ~A TYPE: ~A DEFAULT: ~A~%~%"
            element-name attribute-name type default)))


(defun reset-characters-accumulation (mysax)
  "Resets accumulation of characters so to be able to start from beginning"
  ;; When characters are related to another XML construct, we should
  ;; reset accumulated characters to "". We do it in:
  ;;   * START-ELEMENT
  ;;   * END-ELEMENT
  ;;   * START-CDATA
  ;;   * END-CDATA
  ;;   * PROCESSING-INSTRUCTION
  ;;   * COMMENT
  ;;   * START-DOCUMENT (INITIAL RESET)
  ;;   * END-DOCUMENT (FINAL FLUSH)
  (setf (mysax-characters mysax) ""))


(defun accumulate-characters (mysax new-characters)
  (setf (mysax-characters mysax)
        (concatenate 'string (mysax-characters mysax) new-characters)))


(defun accumulated-characters-exist (mysax)
  (let ((accumulated (mysax-characters mysax)))
    (and (stringp accumulated) (string/= accumulated ""))))


(defun end-accumulated-characters-with-text-node (mysax)
  "Adds text node to MYSAX if there are any accumulated characters and resets them"
  (when (accumulated-characters-exist mysax)
    (let* ((characters (string-trim '(#\Space #\Tab #\Newline) (mysax-characters mysax)))
           (text (model:create-text characters)))
      (model:add-child-node-to-current-elem text (mysax-doc mysax))
      (reset-characters-accumulation mysax))))


(defmethod sax:start-document ((mysax mysax))
  (format t "START-DOCUMENT!~%~%"))


(defmethod sax:start-dtd ((mysax mysax) name public-id system-id)
  (model:set-doc-dtd (mysax-doc mysax)
                     (model:create-dtd :name name :public-id public-id :system-id system-id))
  (format t "START-DTD! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%" name public-id system-id))


(defmethod sax::dtd ((mysax mysax) dtd)  ;; dtd is internal and must be defined
  (format t "DTD! DTD: ~A~%~%" dtd))


(defmethod sax:start-internal-subset ((mysax mysax))
  (format t "START-INTERNAL-SUBSET!~%~%"))


(defmethod sax:element-declaration ((mysax mysax) name model)
  (let ((elem-decl (model:create-elem-decl :name name :model model)))
    (model:add-dtd-item (model:get-doc-dtd (mysax-doc mysax)) elem-decl)
    (format t "ELEMENT-DECLARATION! NAME: ~A MODEL: ~S~%~%" name model)))


(defmethod sax:notation-declaration ((mysax mysax) name public-id system-id)
  (let ((nota-decl (model:create-nota-decl :name name :public-id public-id :system-id system-id)))
    (model:add-dtd-item (model:get-doc-dtd (mysax-doc mysax)) nota-decl)
    (format t "NOTATION-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
            name public-id system-id)))


(defmethod sax:internal-entity-declaration ((mysax mysax) kind name value)
  (let ((int-ent-decl (model:create-int-ent-decl :kind kind :name name :value value)))
    (model:add-dtd-item (model:get-doc-dtd (mysax-doc mysax)) int-ent-decl)
    (format t "INTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A VALUE: ~A~%~%" kind name value)))


(defmethod sax:external-entity-declaration ((mysax mysax) kind name public-id system-id)
  (let ((ext-ent-decl (model:create-ext-ent-decl :kind kind
                                                 :name name
                                                 :public-id public-id
                                                 :system-id system-id)))
    (model:add-dtd-item (model:get-doc-dtd (mysax-doc mysax)) ext-ent-decl)
    (format t "EXTERNAL-ENTITY-DECLARATION! KIND: ~S NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
            kind name public-id system-id)))


(defmethod sax:unparsed-entity-declaration ((mysax mysax) name public-id system-id notation-name)
  (let ((unp-ent-decl (model:create-unp-ent-decl :name name
                                                 :public-id public-id
                                                 :system-id system-id
                                                 :nota-name notation-name)))
    (model:add-dtd-item (model:get-doc-dtd (mysax-doc mysax)) unp-ent-decl)
    (format t "UNPARSED-ENTITY-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A NOTATION-NAME: ~A~%~%"
            name public-id system-id notation-name)))


(defmethod sax:unparsed-internal-subset ((mysax mysax) str)
  (let ((unp-int-subs (model:create-unp-int-subs str)))
    (model:add-dtd-item (model:get-doc-dtd (mysax-doc mysax)) unp-int-subs)
  (format t "UNPARSED-INTERNAL-SUBSET! STR: ~A~%~%" str)))


(defmethod sax:end-internal-subset ((mysax mysax))
  (format t "END-INTERNAL-SUBSET!~%~%"))


(defmethod sax:end-dtd ((mysax mysax))
  (format t "END-DTD!~%~%"))


(defmethod sax:start-prefix-mapping ((mysax mysax) prefix uri)
  (remember-prefix-mapping mysax prefix uri)
  (format t "START-PREFIX-MAPPING! PREFIX: ~A URI: ~A~%~%" prefix uri))


(defmethod sax:end-prefix-mapping ((mysax mysax) prefix)
  (forget-prefix-mappings mysax)
  (format t "END-PREFIX-MAPPING! PREFIX: ~A~%~%" prefix))


(defun adapt-attr (sax-standard-attribute)
  "Creates MODEL:ATTR from SAX:STANDARD-ATTRIBUTE object"
  (model:create-attr :namespace-uri (sax:attribute-namespace-uri sax-standard-attribute)
                     :local-name (sax:attribute-local-name sax-standard-attribute)
                     :qname (sax:attribute-qname sax-standard-attribute)
                     :value (sax:attribute-value sax-standard-attribute)
                     :specified (sax:attribute-specified-p sax-standard-attribute)))


;; (defun current-dir (mysax)
;;   "Current DIR (path) using the current full path from the ELEMS-STACK"
;;   (let* ((elems-stack ;; TODO use full name instead of elem-local-name or?
;;            (reverse (model:doc-elems-stack (mysax-doc mysax))))
;;          (elem-names (mapcar #'model:elem-local-name elems-stack)))
;;     (format nil "~{~A~^/~}" elem-names)))


;; (defun current-dir-with-elem (mysax elem)
;;   "Forms DIR (path) of ELEM using also the current full path from the ELEMS-STACK"
;;   (let* ((cd (current-dir mysax))
;;          (fmt (if (string= cd "") "~*~A" "~A/~A"))) ;; form "a/b" or "b" but not "/b"
;;     (format nil fmt cd (model:elem-local-name elem))))


;; (defun set-node-dir (node mysax)
;;   "Sets DIR of NODE using also ELEMS-STACK from MYSAX"
;;   (let ((node-dir (if (typep node 'model:elem)
;;                       (current-dir-with-elem mysax node)
;;                       (current-dir mysax))))
;;     (setf (model:node-dir node) node-dir)))




;; (defun model:add-child-node-to-current-elem (node mysax)
;;   (check-type node model:node)
;;   (check-type mysax mysax)
;;   (model:add-child-node-to-current-elem node (mysax-doc mysax)))


(defmethod sax:start-element ((mysax mysax) namespace-uri local-name qname attributes)
  (let ((elem (model:create-elem :namespace-uri namespace-uri
                                 :local-name local-name
                                 :qname qname
                                 :prefix-mappings (car (mysax-prefix-mappings mysax))
                                 :attributes (mapcar #'adapt-attr attributes))))
    (end-accumulated-characters-with-text-node mysax)
    (model:add-child-node-to-current-elem elem (mysax-doc mysax))
    (model:enter-elem elem (mysax-doc mysax))
    (format t "START-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A ATTRIBUTES: ~A~%~%"
            namespace-uri local-name qname attributes)))


(defmethod sax:end-element ((mysax mysax) namespace-uri local-name qname)
  ;; (when (accumulated-characters-exist mysax)
  ;;   (let ((text (model:create-text (mysax-characters mysax))))
  ;;     ;; (set-node-dir text mysax)
  ;;     (model:add-child-node-to-current-elem text (mysax-doc mysax))))
  (end-accumulated-characters-with-text-node mysax)
  (model:exit-from-elem (mysax-doc mysax))
  (format t "END-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A~%~%"
          namespace-uri local-name qname))


(defmethod sax:comment ((mysax mysax) data)
  (let ((comment (model:create-comment data)))
    ;; (set-node-dir comment mysax) ;; TODO remove all set-node-dir ?
    (end-accumulated-characters-with-text-node mysax)
    (model:add-child-node-to-current-elem comment (mysax-doc mysax))
    ;; (reset-characters-accumulation mysax)
    (format t "COMMENT! DATA: ~A~%~%" data)))


(defmethod sax:start-cdata ((mysax mysax))
  ;; (reset-characters-accumulation mysax)
  (end-accumulated-characters-with-text-node mysax)
  (format t "START-CDATA!~%~%"))
  ;; (let ((cdata (model:create-cdata)))
  ;;   (set-node-dir cdata mysax)
  ;;   (model:add-child-node-to-current-elem cdata (mysax-doc mysax))
  ;;   (reset-characters-accumulation mysax)
  ;;   (format t "START-CDATA!~%~%")))


(defmethod sax:end-cdata ((mysax mysax))
  (when (accumulated-characters-exist mysax)
    (let ((cdata (model:create-cdata (mysax-characters mysax))))
      (model:add-child-node-to-current-elem cdata (mysax-doc mysax)))
    (reset-characters-accumulation mysax)
    (format t "END-CDATA!~%~%")))
  ;; (symbol-macrolet ((elems-stack (model:doc-elems-stack (mysax-doc mysax)))
  ;;                   (cur-elem (car elems-stack))
  ;;                   (cur-elem-children (model:elem-children cur-elem))
  ;;                   (cur-xml-construct (car (last cur-elem-children))))
  ;;   (when (and (typep cur-xml-construct 'model:cdata)
  ;;              (accumulated-characters-exist mysax))
  ;;     (setf (model:cdata-content cur-xml-construct)
  ;;           (mysax-characters mysax)))
  ;;   (reset-characters-accumulation mysax)
  ;;   (format t "END-CDATA!~%~%")))


(defmethod sax:characters ((mysax mysax) data)
  (unless (every #'whitespace-char-p data)
    (accumulate-characters mysax data))
  (format t "CHARACTERS! DATA: ~A~%~%" data))


(defmethod sax:processing-instruction ((mysax mysax) target data)
  (let ((pinstr (model:create-pinstr :target target :data data)))
    ;; (set-node-dir pinstr mysax)  ;; TODO maybe to unite these 2 calls?
    (end-accumulated-characters-with-text-node mysax)
    (model:add-child-node-to-current-elem pinstr (mysax-doc mysax))
    ;; (reset-characters-accumulation mysax)
    (format t "PROCESSING-INSTRUCTION! TARGET: ~A DATA: ~A~%~%" target data)))


(defmethod sax:end-document ((mysax mysax))
  (format t "END-DOCUMENT!~%~%"))


(defmethod sax:entity-resolver ((mysax mysax) resolver)
  (format t "ENTITY-RESOLVER! RESOLVER: ~A~%~%" resolver))


(defmethod sax:unescaped ((mysax mysax) data)
  (format t "UNESCAPED! DATA: ~A~%~%" data))


(defun decode-ascii-silently (byte-vector)
  (with-output-to-string (out)
    (loop for b across byte-vector
          when (< b 128)               ; ASCII only
            do (write-char (code-char b) out))))


(defun extract-xml-decl (buf)
  "Extracts <?xml ...?> from string buffer BUF. Can returns NIL if it is missing"
  (let ((start (search "<?xml" buf)))
    (when start
      (let ((end (search "?>" buf :start2 start)))
        (when end
          (subseq buf start (+ end 2)))))))


(defun read-xml-decl (stream)
  (let* ((bin-buf (make-array 256 :element-type '(unsigned-byte 8)))
         (read-num (read-sequence bin-buf stream :start 0 :end 256))
         (buf (decode-ascii-silently bin-buf))
         (read-buf (subseq buf 0 read-num))
         (xml-decl (extract-xml-decl read-buf))
         (restored-stream (make-concatenated-stream
                           ;; XXX like (make-string-input-stream read-buf) but for bytes:
                           (flexi-streams:make-in-memory-input-stream bin-buf)
                           stream)))
    (values xml-decl restored-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DESERIALIZE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun deserialize (in-stream)
  "PATH can be NIL, - or a valid path"
  (let ((mysax (make-instance 'mysax))
        (xml-decl-content nil)
        (in-stream1 in-stream))
    (handler-case
        (progn (multiple-value-setq
                   (xml-decl-content in-stream1)
                 (read-xml-decl in-stream))
               (format t "!!!!!!!!!!!!!!!!!! ~A~%" xml-decl-content)
               (if xml-decl-content
                   (model:set-doc-xml-decl (mysax-doc mysax) (model:create-xml-decl xml-decl-content))
                   (model:set-doc-xml-decl (mysax-doc mysax) (model:create-xml-decl "<?xml version=\"1.0\"?>")))
               ;(format t "                   READ ~A~%" (read-line in-stream1))
               (cxml:parse in-stream1 mysax)
               (cons :ok (mysax-doc mysax)))
      (error (x) (cons :fail (format nil "Parsing of input XML failed: ~A" x))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SERIALIZE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serialize-elem-decl-model (model out-stream)
  (cond
    ((stringp model)
     (write-string model out-stream))
    ((eq model :pcdata)
     (write-string "#PCDATA" out-stream))
    ((consp model)
     (case (car model)
       (OR
        (write-char #\( out-stream)
        (loop for item in (cdr model)
              for first = t then nil
              do (unless first
                   (write-string " | " out-stream))
                 (serialize-elem-decl-model item out-stream))
        (write-char #\) out-stream))
       (SEQ
        (write-char #\( out-stream)
        (loop for item in (cdr model)
              for first = t then nil
              do (unless first
                   (write-string ", " out-stream))
                 (serialize-elem-decl-model item out-stream))
        (write-char #\) out-stream))
       (*
        (serialize-elem-decl-model (cadr model) out-stream)
        (write-char #\* out-stream))
       (+
        (serialize-elem-decl-model (cadr model) out-stream)
        (write-char #\+ out-stream))
       (?
        (serialize-elem-decl-model (cadr model) out-stream)
        (write-char #\? out-stream))
       (t
        (error "Unknown content model operator: ~S" (car model)))))))


(defun serialize-xml-decl (xml-decl out-stream)
  (check-type xml-decl (or null model:xml-decl))
  (when xml-decl
    (format out-stream "~A~%" (model:get-xml-decl-content xml-decl))))


(defun serialize-dtd-item (dtd-item out-stream)
  (check-type dtd-item model:dtd-item)
  (etypecase dtd-item
    (model:elem-decl
     (format out-stream "<!ELEMENT ~A "
             (model:get-elem-decl-name dtd-item))
     (serialize-elem-decl-model (model:get-elem-decl-model dtd-item) out-stream)
     (format out-stream ">~%"))
    (model:attr-decl
     (format out-stream "<!ATTLIST ~A ~A ~A ~A>~%"
             (model:get-attr-decl-elem-name dtd-item)
             (model:get-attr-decl-attr-name dtd-item)
             (model:get-attr-decl-type dtd-item)
             (model:get-attr-decl-default dtd-item)))
    (model:nota-decl
     (if (model:get-nota-decl-public-id dtd-item)
         (format out-stream
                 "<!NOTATION ~A PUBLIC \"~A\" \"~A\">~%"
                 (model:get-nota-decl-name dtd-item)
                 (model:get-nota-decl-public-id dtd-item)
                 (model:get-nota-decl-system-id dtd-item))
         (format out-stream
                 "<!NOTATION ~A SYSTEM \"~A\">~%"
                 (model:get-nota-decl-name dtd-item)
                 (model:get-nota-decl-system-id dtd-item))))
    (model:int-ent-decl
     (ecase (model:get-int-ent-decl-kind dtd-item)
       (:general
        (format out-stream
                "<!ENTITY ~A \"~A\">~%"
                (model:get-int-ent-decl-name dtd-item)
                (model:get-int-ent-decl-value dtd-item)))
       (:parameter
        (format out-stream
                "<!ENTITY % ~A \"~A\">~%"
                (model:get-int-ent-decl-name dtd-item)
                (model:get-int-ent-decl-value dtd-item)))))
    (model:ext-ent-decl
     (ecase (model:get-ext-ent-decl-kind dtd-item)
       (:general
        (if (model:get-ext-ent-decl-public-id dtd-item)
            (format out-stream "<!ENTITY ~A PUBLIC ~A \"~A\">~%"
                    (model:get-ext-ent-decl-name dtd-item)
                    (model:get-ext-ent-decl-public-id dtd-item)
                    (model:get-ext-ent-decl-system-id dtd-item))
            (format out-stream "<!ENTITY ~A SYSTEM \"~A\">~%"
                    (model:get-ext-ent-decl-name dtd-item)
                    (model:get-ext-ent-decl-system-id dtd-item))))
       (:parameter
        (if (model:get-ext-ent-decl-public-id dtd-item)
            (format out-stream "<!ENTITY % ~A PUBLIC ~A \"~A\">~%"
                    (model:get-ext-ent-decl-name dtd-item)
                    (model:get-ext-ent-decl-public-id dtd-item)
                    (model:get-ext-ent-decl-system-id dtd-item))
            (format
             out-stream "<!ENTITY % ~A SYSTEM \"~A\">~%"
             (model:get-ext-ent-decl-name dtd-item)
             (model:get-ext-ent-decl-system-id dtd-item))))))
    (model:unp-ent-decl
     (if (model:get-unp-ent-decl-public-id dtd-item)
         (format out-stream "<!ENTITY ~A PUBLIC \"~A\" \"~A\" NDATA ~A>~%"
                 (model:get-unp-ent-decl-name dtd-item)
                 (model:get-unp-ent-decl-public-id dtd-item)
                 (model:get-unp-ent-decl-system-id dtd-item)
                 (model:get-unp-ent-decl-nota-name dtd-item))
         (format out-stream "<!ENTITY ~A SYSTEM \"~A\" NDATA ~A>~%"
                 (model:get-unp-ent-decl-name dtd-item)
                 (model:get-unp-ent-decl-system-id dtd-item)
                 (model:get-unp-ent-decl-nota-name dtd-item))))
    (model:unp-int-subs
     (format out-stream "[~A]" (model:get-unp-int-subs-content dtd-item)))))


(defun serialize-doc-dtd (doc-dtd out-stream)
  (check-type doc-dtd (or null model:dtd))
  (when doc-dtd
    (cond ((model:get-dtd-public-id doc-dtd)
           (format out-stream "<!DOCTYPE ~A PUBLIC \"~A\" \"~A\""
                   (model:get-dtd-name doc-dtd)
                   (model:get-dtd-public-id doc-dtd)
                   (model:get-dtd-system-id doc-dtd)))
          ((model:get-dtd-system-id doc-dtd)
           (format out-stream "<!DOCTYPE ~A SYSTEM \"~A\""
                   (model:get-dtd-name doc-dtd)
                   (model:get-dtd-system-id doc-dtd)))
          (t
           (format out-stream "<!DOCTYPE ~A"
                   (model:get-dtd-name doc-dtd))))
    (if (model:get-dtd-items doc-dtd)
        (progn (format out-stream " [~%")
               (dolist (item (model:get-dtd-items doc-dtd))
                 (serialize-dtd-item item out-stream))
               (format out-stream "]>~%"))
        (format out-stream ">~%"))))


(defun serialize-node (node out-stream)
  (check-type node model:node)
  (etypecase node
    (model:text (format out-stream "~A~%"
                        (escape-unsafe-xml-text
                         (model:get-text-content node))))
    (model:pinstr (format out-stream "~A~A~@[ ~A~]~A~%"
                          (model:get-node-open-by node)
                          (model:get-pinstr-target node)
                          (model:get-pinstr-data node)
                          (model:get-node-close-by node)))
    (model:cdata (when (string/= (model:get-cdata-content node) "")
                   (format out-stream "~A~A~A~%"
                           (model:get-node-open-by node)
                           (model:get-cdata-content node)
                           (model:get-node-close-by node))))
    (model:comment (when (string/= (model:get-comment-content node) "")
                     (format out-stream "~A~A~A~%"
                             (model:get-node-open-by node)
                             (model:get-comment-content node)
                             (model:get-node-close-by node))))
    (model:elem (serialize-elem node out-stream))))


(defun escape-unsafe-xml-text (string)
  (check-type string string)
  (flet ((escape-char (ch)
           (cond
             ((char= ch #\&) "&amp;")
             ((char= ch #\<) "&lt;")
             ((char= ch #\>) "&gt;")
             ((char= ch #\") "&quot;")
             ((> (char-code ch) 127)
              (format nil "&#x~X;" (char-code ch)))
             (t (string ch)))))
    (with-output-to-string (safe-string)
      (loop for ch across string
            do (write-string (escape-char ch) safe-string)))))


(defun serialize-attr (attr out-stream)
  (check-type attr model:attr)
  (format out-stream " ~A=\"~A\""
          (or (model:get-attr-qname attr)
              (model:get-attr-local-name attr))
          (escape-unsafe-xml-text
           (model:get-attr-value attr))))



(defun serialize-elem (elem out-stream)
  (check-type elem model:elem)
  (format out-stream "<~A" (model:get-elem-qname elem))
  (model:over-prefix-mappings
   (model:get-elem-uniq-prefix-mappings elem)
   :do #'(lambda (pmi) ;; arg is a CONS
           (destructuring-bind (prefix . uri) pmi
             (if (non-empty-string-p prefix)
                 (format out-stream " xmlns:~A=\"~A\"" prefix uri)
                 (format out-stream " xmlns=\"~A\"" uri)))))
  ;;(format out-stream " ~A" (model:get-elem-namespace-uri elem))
  (dolist (attr (model:get-elem-attributes elem))
    (serialize-attr attr out-stream))
  (if (= 0 (model:get-elem-children-num elem))
      (format out-stream "/>~%")
      (progn
        (format out-stream ">~%")
        (model:over-elem-children
         elem
         :do #'(lambda (child-node) (serialize-node child-node out-stream)))
        (format out-stream "</~A>~%" (model:get-elem-qname elem)))))



(defun serialize (doc out-stream)
  (check-type doc model:doc)
  ;; (inspect doc)
  (let* ((xml-decl (model:get-doc-xml-decl doc))
         (doc-dtd (model:get-doc-dtd doc)))
    (serialize-xml-decl xml-decl out-stream)
    (serialize-doc-dtd doc-dtd out-stream)
    (serialize-node (model:get-doc-root doc) out-stream)))
