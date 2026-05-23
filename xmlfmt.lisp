;; TERMINOLOGY:
;;
;; XML construct ([cur-]xml-construct) - any construction/item inside XML
;; DTD, items in DTD, elements, comments

(in-package :xmlfmt)


(defclass mysax (sax:abstract-handler)
  ((doc :type model:doc
        :documentation ""
        :initform (make-instance 'model:doc)
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
    (model:add-dtd-item (model:doc-dtd (mysax-doc mysax)) attr-decl)
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
    (model:add-dtd-item (model:doc-dtd (mysax-doc mysax)) elem-decl)
    (format t "ELEMENT-DECLARATION! NAME: ~A MODEL: ~A~%~%" name model)))


(defmethod sax:notation-declaration ((mysax mysax) name public-id system-id)
  (let ((nota-decl (model:create-nota-decl :name name :public-id public-id :system-id system-id)))
    (model:add-dtd-item (model:doc-dtd (mysax-doc mysax)) nota-decl)
    (format t "NOTATION-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
            name public-id system-id)))


(defmethod sax:internal-entity-declaration ((mysax mysax) kind name value)
  (let ((int-ent-decl (model:create-int-ent-decl :kind kind :name name :value value)))
    (model:add-dtd-item (model:doc-dtd (mysax-doc mysax)) int-ent-decl)
    (format t "INTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A VALUE: ~A~%~%" kind name value)))


(defmethod sax:external-entity-declaration ((mysax mysax) kind name public-id system-id)
  (let ((ext-ent-decl (model:create-ext-ent-decl :kind kind
                                                 :name name
                                                 :public-id public-id
                                                 :system-id system-id)))
    (model:add-dtd-item (model:doc-dtd (mysax-doc mysax)) ext-ent-decl)
    (format t "EXTERNAL-ENTITY-DECLARATION! KIND: ~A NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A~%~%"
            kind name public-id system-id)))


(defmethod sax:unparsed-entity-declaration ((mysax mysax) name public-id system-id notation-name)
  (let ((unp-ent-decl (model:create-unp-ent-decl :name name
                                                 :public-id public-id
                                                 :system-id system-id
                                                 :nota-name notation-name)))
    (model:add-dtd-item (model:doc-dtd (mysax-doc mysax)) unp-ent-decl)
    (format t "UNPARSED-ENTITY-DECLARATION! NAME: ~A PUBLIC-ID: ~A SYSTEM-ID: ~A NOTATION-NAME: ~A~%~%"
            name public-id system-id notation-name)))


(defmethod sax:unparsed-internal-subset ((mysax mysax) str)
  (let ((unp-int-subs (model:create-unp-int-subs :content str)))
    (model:add-dtd-item (model:doc-dtd (mysax-doc mysax)) unp-int-subs)
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


(defun numerate-elem-siblings (elem)
  (let ((counters nil)
        (deferred-updates nil))
    (labels ((calc-child-id (child)
               (format nil "~A--~A" (type-of child) (model:calc-node-dir child :join-by "")))
             (defer-child-update (child)
               (when (typep child 'model:node)
                 (let* ((child-id (calc-child-id child))
                        (child-counter (assoc child-id counters :test #'equal))
                        (child-num (or (cdr child-counter) 0)))
                   ;; (format t "!!!!!!!!!!!!!!!!!!!!! ~A  ~A (~A): ~A~%"
                   ;;         (type-of child) (model:node-dir child) child-id child-num)
                   ;; (setf (model:node-idx child) child-num)
                   (push (cons child child-num) deferred-updates)
                   (if child-counter
                       (incf (cdr child-counter))
                       (push (cons child-id 1) counters)))))
             (execute-deferred-update (deferred-update)
               (let* ((child (car deferred-update))
                      (child-num (cdr deferred-update))
                      (child-id (calc-child-id child)))
                 (when (> (cdr (assoc child-id counters :test #'equal)) 1)
                   (model:set-node-idx child child-num)))))
      (model:over-elem-children elem :do defer-child-update)
      (dolist (deferred-update (reverse deferred-updates))
        (execute-deferred-update deferred-update))
      ;;(format t "         !!!!! AFTER: ~A~%" (mapcar #'model:node-idx (model:elem-children elem)))
      )))


(defun add-node-as-child (node mysax) ;; TODO
  (symbol-macrolet ((elems-stack (model:doc-elems-stack (mysax-doc mysax)))
                    (cur-elem (car elems-stack))
                    (cur-elem-children (model:elem-children cur-elem)))
    (when elems-stack (setf cur-elem-children
                              (append cur-elem-children
                                      (list node))))
    (when (typep cur-elem 'model:elem)
      (numerate-elem-siblings cur-elem))))


(defun enter-elem (elem mysax)
  "Pushes ELEM to ELEMS-STACK making it the current element"
  (symbol-macrolet ((elems-stack (model:doc-elems-stack (mysax-doc mysax))))
    (push elem elems-stack)))


(defun exit-from-elem (mysax)
  (symbol-macrolet ((elems-stack (model:doc-elems-stack (mysax-doc mysax))))
    (when (cdr elems-stack)
      (pop elems-stack)))) ;; TODO numerate same elems in children


(defmethod sax:start-element ((mysax mysax) namespace-uri local-name qname attributes)
  (let ((elem (make-instance 'model:elem :namespace-uri namespace-uri
                                         :local-name local-name
                                         :qname qname
                                         :prefix-mappings (car (mysax-prefix-mappings mysax))
                                         :attributes (mapcar #'adapt-attr attributes))))
    (set-node-dir elem mysax)
    (add-node-as-child elem mysax)
    (enter-elem elem mysax)
    (reset-characters-accumulation mysax)
    (format t "START-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A ATTRIBUTES: ~A~%~%"
            namespace-uri local-name qname attributes)))


(defmethod sax:end-element ((mysax mysax) namespace-uri local-name qname)
  (when (accumulated-characters-exist mysax)
    (let ((text (make-instance 'model:text :content (mysax-characters mysax))))
      (set-node-dir text mysax)
      (add-node-as-child text mysax)))
  (exit-from-elem mysax)
  (reset-characters-accumulation mysax)
  (format t "END-ELEMENT! NAMESPACE-URI: ~A LOCAL-NAME: ~A QNAME: ~A~%~%"
          namespace-uri local-name qname))


(defmethod sax:comment ((mysax mysax) data)
  (let ((comment (make-instance 'model:comment :content data)))
    (set-node-dir comment mysax)
    (add-node-as-child comment mysax)
    (reset-characters-accumulation mysax)
    (format t "COMMENT! DATA: ~A~%~%" data)))


(defmethod sax:start-cdata ((mysax mysax))
  (let ((cdata (make-instance 'model:cdata)))
    (set-node-dir cdata mysax)
    (add-node-as-child cdata mysax)
    (reset-characters-accumulation mysax)
    (format t "START-CDATA!~%~%")))


(defmethod sax:end-cdata ((mysax mysax))
  (symbol-macrolet ((elems-stack (model:doc-elems-stack (mysax-doc mysax)))
                    (cur-elem (car elems-stack))
                    (cur-elem-children (model:elem-children cur-elem))
                    (cur-xml-construct (car (last cur-elem-children))))
    (when (and (typep cur-xml-construct 'model:cdata)
               (accumulated-characters-exist mysax))
      (setf (model:cdata-content cur-xml-construct)
            (mysax-characters mysax)))
    (reset-characters-accumulation mysax)
    (format t "END-CDATA!~%~%")))


(defmethod sax:characters ((mysax mysax) data)
  (unless (every #'whitespace-char-p data)
    (accumulate-characters mysax data))
  (format t "CHARACTERS! DATA: ~A~%~%" data))


(defmethod sax:processing-instruction ((mysax mysax) target data)
  (let ((pinstr (make-instance 'model:pinstr :target target :content data)))
    (set-node-dir pinstr mysax)  ;; TODO maybe to unite these 2 calls?
    (add-node-as-child pinstr mysax)
    (reset-characters-accumulation mysax)
    (format t "PROCESSING-INSTRUCTION! TARGET: ~A DATA: ~A~%~%" target data)))


(defmethod sax:end-document ((mysax mysax))
  (format t "END-DOCUMENT!~%~%"))


(defmethod sax:entity-resolver ((mysax mysax) resolver)
  (format t "ENTITY-RESOLVER! RESOLVER: ~A~%~%" resolver))


(defmethod sax:unescaped ((mysax mysax) data)
  (format t "UNESCAPED! DATA: ~A~%~%" data))


(defun parse-xml (path)
  (let ((mysax (make-instance 'mysax)))
    (handler-case
        (progn (call-with-input-stream
                path
                (lambda (f) (cxml:parse f mysax)))
               (cons :ok (mysax-doc mysax)))
      (error (x) (cons :fail (format nil "Parsing of XML '~A' failed: ~A" path x))))))
