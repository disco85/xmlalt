(in-package :model)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct uri
  (value "" :type string))


(deftype elem-decl-model ()
  '(or string keyword cons))


(deftype attr-decl-type ()
  '(or
    (member :CDATA :ID :IDREF :IDREFS
            :ENTITY :ENTITIES
            :NMTOKEN :NMTOKENS)
    (cons (member :NOTATION :ENUMERATION)
     list)))


(deftype attr-decl-default ()
  '(or
    (member :REQUIRED :IMPLIED)
    (cons (member :FIXED :DEFAULT)
     (cons string null))))


(defstruct node
  (idx nil :type (or null (integer 0)))
  (open-by "<" :type string)
  (close-by ">" :type string)
  (parent nil :type (or null node)))


(defstruct attr
  (namespace-uri nil :type (or null uri))
  (local-name nil :type (or null string))
  (qname nil :type (or null string))
  (value nil :type (or null string))
  (specified nil :type boolean))


(defstruct (text (:include node
                  (open-by "")
                  (close-by ""))
                 ;; XXX to avoid cyclic print due to PARENT/CHILDREN:
                 (:print-function print-text))
  (content "" :type string))


(defstruct (pinstr (:include node
                    (open-by "<?")
                    (close-by "?>")))
  (target "" :type string)
  (data nil :type (or null string)))


(defstruct (cdata (:include node
                   (open-by "<![CDATA[")
                   (close-by "]]>")))
  (content "" :type string))


(defstruct (comment (:include node
                     (open-by "<!--")
                     (close-by "-->")))
  (content "" :type string))


(defstruct prefix-mappings
  (items nil :type list))


(defstruct (elem (:include node)
                 ;; XXX to avoid cyclic print due to PARENT/CHILDREN:
                 (:print-function print-elem))
  (namespace-uri nil :type (or null uri))
  (local-name nil :type (or null string))
  (qname nil :type (or null string))
  (prefix-mappings nil :type (or null prefix-mappings))
  (attributes nil :type list)
  (children nil :type list))


(defstruct doctype
  (content "" :type string))


(defstruct xml-decl
  (content "" :type string))


(defstruct dtd-item)


(defstruct (elem-decl (:include dtd-item))
  (name "" :type string)
  (model nil :type elem-decl-model))


(defstruct (attr-decl (:include dtd-item))
  (elem-name "" :type string)
  (attr-name "" :type string)
  (type :CDATA :type attr-decl-type)
  (default :IMPLIED :type attr-decl-default))


(defstruct (nota-decl (:include dtd-item))
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))


(defstruct (int-ent-decl (:include dtd-item))
  (kind :general :type (member :general :parameter))
  (name "" :type string)
  (value "" :type string))


(defstruct (ext-ent-decl (:include dtd-item))
  (kind :general :type (member :general :parameter))
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))


(defstruct (unp-ent-decl (:include dtd-item))
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type string)
  (nota-name "" :type string))


(defstruct (unp-int-subs (:include dtd-item))
  (content "" :type string))


;; TODO elems-stack must be list of elem|dtd|dtd-item??|?
(defstruct (dtd (:include node
                 (open-by "")
                 (close-by "")))
  (items nil :type list)
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))


(defstruct doc
  (xml-decl nil :type (or null xml-decl))
  (dtd nil :type (or null dtd))
  (elems-stack nil :type list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun try-as-uri (s)
  (typecase s
    (null s)
    (string (create-uri s))
    (uri s)
    (t (error (format nil "Invalid value for TRY-AS-URI (type ~A): ~A" s (type-of s))))))


(defun create-uri (uri-value)
  (assert (non-empty-string-p uri-value))
  (make-uri :value uri-value))


(defun write-uri (uri &optional stream)
  (check-type uri uri)
  (format stream "~A" (uri-value uri)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NODE API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-node (&key idx parent)
  (make-node :idx idx :parent parent))


(defun set-node-idx (node idx)
  (check-type node node)
  (assert (and (integerp idx) (>= idx 0)))
  (setf (node-idx node) idx))


(defun get-node-idx (node)
  (check-type node node)
  (node-idx node))


(defun calc-node-dir (node &key with-idx-as non-elem-name-as join-by without-root)
  "Collects DIR of a NODE (adding IDX, if WITH-IDX-AS was passed) of every
ELEM en route and returns the result as a list of strings. But if
JOIN-BY was passed as some STRING, then returns it as a STRING joining
components by this delimiter. Non ELEM components are handled by
optional NON-ELEM-NAME-AS that must return a string for such one -
without it such dir component will be skipped. WITH-IDX-AS converts
integer IDX to STRING"
  (check-type node node)
  (check-type join-by (or null string))
  (labels ((prep-join-fmt (delim)
             "Prepares FORMAT string able to join items by DELIM"
             (concatenate 'string "~{~A~^" delim "~}"))
           (calc-node-idx (n)
             (if without-root
               (1- (node-idx n))
               (node-idx n)))
           (cons-idx-if (n lst)
             "Adds IDX of a NODE N to the front of list LST if WITH-IDX"
             (if (and with-idx-as (calc-node-idx n))
               (cons (funcall with-idx-as (calc-node-idx n))
                     lst)
               lst))
           (collect-dirs (n dirs)
             "Recursively collects DIR from a node N to the top parent"
             (declare (type (or null (cons string *)) dirs))
             (typecase n
               (null dirs)
               (elem (collect-dirs (node-parent n)
                                   (cons-idx-if n
                                                (cons (elem-local-name n)
                                                      dirs))))
               (node (if non-elem-name-as
                       (collect-dirs (node-parent n)
                                     (cons-idx-if n
                                                  (cons (funcall non-elem-name-as n)
                                                        dirs)))
                       (collect-dirs (node-parent n) dirs)))
               (t dirs))))
    (let* ((dirs0 (collect-dirs node nil))
           (dirs (cond ((and without-root with-idx-as) (cddr dirs0))
                       (without-root                   (cdr dirs0))
                       (t                              dirs0))))
      (if join-by
        (format nil (prep-join-fmt join-by) dirs)
        dirs))))


(defun %add-child-node-to-elem (child-node parent-elem)
  "Adds a CHILD-NODE to ELEM TO-ELEM"
  (check-type child-node node)
  (check-type parent-elem elem)
  (assert (not (eq child-node parent-elem)))
  (symbol-macrolet ((children (elem-children parent-elem)))
    (setf children (append children (list child-node)))
    (setf (node-parent child-node) parent-elem))
  (%numerate-elem-children parent-elem))


(defun add-child-node-to-current-elem (child-node doc)
  "Adds CHILD-NODE to the current ELEM (tracked in DOC)"
  (check-type child-node node)
  (check-type doc doc)
  (let* ((elems-stack (doc-elems-stack doc))
         (cur-elem (car elems-stack)))
    (when cur-elem (%add-child-node-to-elem child-node cur-elem))))



(defun get-node-open-by (node)
  (check-type node node)
  (node-open-by node))



(defun get-node-close-by (node)
  (check-type node node)
  (node-close-by node))



(defun get-node-parent (node)
  (check-type node node)
  (node-parent node))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ATTR API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-attr (&key namespace-uri
                      (local-name nil local-name-p)
                      (qname nil qname-p)
                      value
                      specified)
  (assert (or (null namespace-uri) (non-empty-string-p namespace-uri)))
  (assert (or (null local-name) (non-empty-string-p local-name)))
  (assert (or (null qname) (non-empty-string-p qname)))
  (assert (or local-name-p qname-p))
  (make-attr :namespace-uri (when namespace-uri
                              (create-uri namespace-uri))
             :local-name local-name
             :qname qname
             :value value
             :specified specified))


(defun get-attr-namespace-uri (attr)
  (check-type attr attr)
  (attr-namespace-uri attr))


(defun get-attr-local-name (attr)
  (check-type attr attr)
  (attr-local-name attr))


(defun get-attr-qname (attr)
  (check-type attr attr)
  (attr-qname attr))


(defun get-attr-value (attr)
  (check-type attr attr)
  (attr-value attr))


(defun get-attr-specified (attr)
  (check-type attr attr)
  (attr-specified attr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TEXT API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-text (content)
  (assert (non-empty-string-p content))
  (make-text :content content))


(defun print-text (obj stream depth)
  (declare (ignore depth))
  (format stream "#<TEXT ~S>"
          (text-content obj)))



(defun get-text-content (text)
  (check-type text text)
  (text-content text))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PINSTR API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-pinstr (&key target data)
  (assert (non-empty-string-p target))
  (assert (or (null data)
              (non-empty-string-p data)))
  (make-pinstr :target target :data data))


(defun get-pinstr-target (pinstr)
  (check-type pinstr pinstr)
  (pinstr-target pinstr))


(defun get-pinstr-data (pinstr)
  (check-type pinstr pinstr)
  (pinstr-data pinstr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CDATA API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-cdata (content)
  (assert (non-empty-string-p content))
  (make-cdata :content content))


(defun get-cdata-content (cdata)
  (check-type cdata cdata)
  (cdata-content cdata))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; COMMENT API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-comment (content)
  (assert (non-empty-string-p content))
  (make-comment :content content))


(defun get-comment-content (comment)
  (check-type comment comment)
  (comment-content comment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PREFIX MAPPINGS API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-prefix-mappings (&optional items)
  (make-prefix-mappings :items items))


(defun add-prefix-mappings (prefix-mappings &rest new-pairs)
  (setf (prefix-mappings-items prefix-mappings)
        (append new-pairs
                (prefix-mappings-items prefix-mappings))))


(defun over-prefix-mappings (prefix-mappings
                             &key (collect nil collect-p) (do nil do-p))
  (check-type prefix-mappings (or null prefix-mappings))
  (assert (not (and collect-p do-p)))
  (let ((prefix-mappings-items (when prefix-mappings
                                 (prefix-mappings-items prefix-mappings))))
    (cond (collect-p (mapcar collect prefix-mappings-items))
          (do-p      (dolist (pm prefix-mappings-items)
                       (funcall do pm)))
          (t (error "Pass either :COLLECT or :DO")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ELEM API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-elem (&key namespace-uri
                      (local-name nil local-name-p)
                      (qname nil qname-p)
                      prefix-mappings
                      attributes
                      children)
  (assert (or (null namespace-uri)
              (non-empty-string-p namespace-uri)))
  (assert (or (null local-name)
              (non-empty-string-p local-name)))
  (assert (or (null qname)
              (non-empty-string-p qname)))
  (assert (or local-name-p qname-p))
  (make-elem :namespace-uri (try-as-uri namespace-uri)
             :local-name local-name
             :qname qname
             :prefix-mappings prefix-mappings
             :attributes attributes
             :children children))


(defun print-elem (obj stream depth)
  (declare (ignore depth))
  (format stream "#<ELEM ~A>"
          (elem-qname obj)))



(defun get-elem-children-num (elem)
  (check-type elem elem)
  (length (elem-children elem)))


(defun over-elem-children (elem
                           &key
                             (collect nil collect-p)
                             (do nil do-p))
  (check-type elem elem)
  (assert (not (and collect-p do-p)))
  (cond (collect-p (mapcar collect
                           (elem-children elem)))
        (do-p (dolist (child (elem-children elem))
                (funcall do child)))
        (t (error "Pass either :COLLECT or :DO"))))


(defun %numerate-elem-children (elem)
  "Refreshes NODE-IDX field of ....."
  (check-type elem elem)
  (let ((counters nil)
        (deferred-updates nil))
    (labels ((calc-child-id (child)
               (cons (type-of child)
                     (when (typep child 'elem)
                       (elem-local-name child)))
               ;; (format nil "~A--~A"
               ;;         (type-of child)
               ;;         (calc-node-dir child :join-by ""))
               )
             (defer-child-update (child)
               (when (typep child 'node)
                 (let* ((child-id (calc-child-id child))
                        (child-counter (assoc child-id counters :test #'equal))
                        (child-num (or (cdr child-counter) 0)))
                   (push (cons child child-num) deferred-updates)
                   (if child-counter
                       (incf (cdr child-counter))
                       (push (cons child-id 1) counters)))))
             (execute-deferred-update (deferred-update)
               (let* ((child (car deferred-update))
                      (child-num (cdr deferred-update))
                      (child-id (calc-child-id child)))
                 (when (> (cdr (assoc child-id counters :test #'equal)) 1)
                   (set-node-idx child child-num)))))
      (over-elem-children elem :do #'defer-child-update)
      (dolist (deferred-update (reverse deferred-updates))
        (execute-deferred-update deferred-update))
      )))


(defun enter-elem (elem doc)
  "Pushes ELEM to ELEMS-STACK making it the current element"
  (check-type elem elem)
  (check-type doc doc)
    (push elem (doc-elems-stack doc)))


(defun exit-from-elem (doc)
  "Pops (like Linux popd(1) command) current ELEM from the stack of XML elements"
  (symbol-macrolet ((elems-stack (doc-elems-stack doc)))
    (when (cdr elems-stack)
      (pop elems-stack))))


(defun get-elem-namespace-uri (elem)
  (check-type elem elem)
  (elem-namespace-uri elem))


(defun get-elem-local-name (elem)
  (check-type elem elem)
  (elem-local-name elem))


(defun get-elem-qname (elem)
  (check-type elem elem)
  (elem-qname elem))


(defun get-elem-prefix-mappings (elem)
  (check-type elem elem)
  (elem-prefix-mappings elem))


(defun get-elem-uniq-prefix-mappings (elem)
  "Returns PREFIX-MAPPINGS of ELEM without PREFIX-MAPPINGS of ELEM's PARENT"
  (let* ((elem-parent (get-node-parent elem))
         (elem-parent-prefix-mappings (when elem-parent
                                        (get-elem-prefix-mappings elem-parent)))
         (elem-parent-prefix-mappings-items (when elem-parent-prefix-mappings
                                              (prefix-mappings-items elem-parent-prefix-mappings)))
         (elem-prefix-mappings (get-elem-prefix-mappings elem))
         (elem-prefix-mappings-items (when elem-prefix-mappings
                                       (prefix-mappings-items elem-prefix-mappings)))
         (uniq-elem-prefix-mappings (set-difference
                                     elem-prefix-mappings-items
                                     elem-parent-prefix-mappings-items
                                     :test #'equal)))))


(defun get-elem-attributes (elem)
  (check-type elem elem)
  (elem-attributes elem))


(defun get-elem-children (elem)
  (check-type elem elem)
  (elem-children elem))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DOCTYPE API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-doctype (content)
  (assert (non-empty-string-p content))
  (make-doctype :content content))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XML-DECL API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-xml-decl (content)
  (assert (non-empty-string-p content))
  (make-xml-decl :content content))


(defun get-xml-decl-content (xml-decl)
  (check-type xml-decl xml-decl)
  (xml-decl-content xml-decl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ELEM-DECL API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-elem-decl (&key name model)
  (assert (non-empty-string-p name))
  (assert (or (consp model)
              (keywordp model)
              (non-empty-string-p model)))
  (make-elem-decl :name name
                  :model model))


(defun get-elem-decl-name (elem-decl)
  (check-type elem-decl elem-decl)
  (elem-decl-name elem-decl))


(defun get-elem-decl-model (elem-decl)
  (check-type elem-decl elem-decl)
  (elem-decl-model elem-decl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ATTR-DECL API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-attr-decl (&key elem-name attr-name type default)
  (assert (non-empty-string-p elem-name))
  (assert (non-empty-string-p attr-name))
  (check-type type attr-decl-type)
  (check-type default attr-decl-default)
  (make-attr-decl :elem-name elem-name
                  :attr-name attr-name
                  :type type
                  :default default))


(defun get-attr-decl-elem-name (attr-decl)
  (check-type attr-decl attr-decl)
  (attr-decl-elem-name attr-decl))


(defun get-attr-decl-attr-name (attr-decl)
  (check-type attr-decl attr-decl)
  (attr-decl-attr-name attr-decl))


(defun get-attr-decl-type (attr-decl)
  (check-type attr-decl attr-decl)
  (attr-decl-type attr-decl))


(defun get-attr-decl-default (attr-decl)
  (check-type attr-decl attr-decl)
  (attr-decl-default attr-decl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTA-DECL API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-nota-decl (&key
                           name
                           (public-id nil public-id-p)
                           (system-id nil system-id-p))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-nota-decl :name name
                  :public-id public-id
                  :system-id system-id))


(defun get-nota-decl-name (nota-decl)
  (check-type nota-decl nota-decl)
  (nota-decl-name nota-decl))


(defun get-nota-decl-public-id (nota-decl)
  (check-type nota-decl nota-decl)
  (nota-decl-public-id nota-decl))


(defun get-nota-decl-system-id (nota-decl)
  (check-type nota-decl nota-decl)
  (nota-decl-system-id nota-decl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INT-ENT-DECL API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-int-ent-decl (&key kind name value)
  (assert (or (keywordp kind)
              (non-empty-string-p kind)))
  (assert (non-empty-string-p name))
  (assert (non-empty-string-p value))
  (make-int-ent-decl :kind kind
                     :name name
                     :value value))


(defun get-int-ent-decl-kind (int-ent-decl)
  (check-type int-ent-decl int-ent-decl)
  (int-ent-decl-kind int-ent-decl))


(defun get-int-ent-decl-name (int-ent-decl)
  (check-type int-ent-decl int-ent-decl)
  (int-ent-decl-name int-ent-decl))


(defun get-int-ent-decl-value (int-ent-decl)
  (check-type int-ent-decl int-ent-decl)
  (int-ent-decl-value int-ent-decl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXT-ENT-DECL API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-ext-ent-decl (&key
                              kind
                              name
                              (public-id nil public-id-p)
                              (system-id nil system-id-p))
  (assert (or (keywordp kind)
              (non-empty-string-p kind)))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-ext-ent-decl :kind kind
                     :name name
                     :public-id public-id
                     :system-id system-id))


(defun get-ext-ent-decl-kind (ext-ent-decl)
  (check-type ext-ent-decl ext-ent-decl)
  (ext-ent-decl-kind ext-ent-decl))


(defun get-ext-ent-decl-name (ext-ent-decl)
  (check-type ext-ent-decl ext-ent-decl)
  (ext-ent-decl-name ext-ent-decl))


(defun get-ext-ent-decl-public-id (ext-ent-decl)
  (check-type ext-ent-decl ext-ent-decl)
  (ext-ent-decl-public-id ext-ent-decl))


(defun get-ext-ent-decl-system-id (ext-ent-decl)
  (check-type ext-ent-decl ext-ent-decl)
  (ext-ent-decl-system-id ext-ent-decl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UNP-ENT-DECL API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-unp-ent-decl (&key
                              name
                              (public-id nil public-id-p)
                              (system-id nil system-id-p) nota-name)
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (assert (non-empty-string-p nota-name))
  (make-unp-ent-decl :name name
                     :public-id public-id
                     :system-id system-id
                     :nota-name nota-name))


(defun get-unp-ent-decl-name (unp-ent-decl)
  (check-type unp-ent-decl unp-ent-decl)
  (unp-ent-decl-name unp-ent-decl))


(defun get-unp-ent-decl-public-id (unp-ent-decl)
  (check-type unp-ent-decl unp-ent-decl)
  (unp-ent-decl-public-id unp-ent-decl))


(defun get-unp-ent-decl-system-id (unp-ent-decl)
  (check-type unp-ent-decl unp-ent-decl)
  (unp-ent-decl-system-id unp-ent-decl))


(defun get-unp-ent-decl-nota-name (unp-ent-decl)
  (check-type unp-ent-decl unp-ent-decl)
  (unp-ent-decl-nota-name unp-ent-decl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UNP-INT-SUBS API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-unp-int-subs (content)
  (assert (non-empty-string-p content))
  (make-unp-int-subs :content content))


(defun get-unp-int-subs-content (unp-int-subs)
  (check-type unp-int-subs unp-int-subs)
  (unp-int-subs-content unp-int-subs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DTD API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-dtd (&key
                     items
                     name
                     (public-id nil public-id-p)
                     (system-id nil system-id-p))
  (assert (every (lambda (it) (dtd-item-p it)) items))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-dtd :items items
            :name name
            :public-id public-id
            :system-id system-id))


(defun add-dtd-item (dtd item)
  (check-type dtd dtd)
  (check-type item dtd-item)
  (push item (dtd-items dtd)))


(defun get-dtd-items (dtd)
  (check-type dtd dtd)
  (reverse (dtd-items dtd)))


(defun get-dtd-name (dtd)
  (check-type dtd dtd)
  (dtd-name dtd))


(defun get-dtd-public-id (dtd)
  (check-type dtd dtd)
  (dtd-public-id dtd))


(defun get-dtd-system-id (dtd)
  (check-type dtd dtd)
  (dtd-system-id dtd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DOC API
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-doc ()
  (make-doc))


(defun set-doc-dtd (doc dtd)
  (check-type doc doc)
  (check-type dtd dtd)
  (setf (doc-dtd doc) dtd))


(defun get-doc-root (doc)
  "The root of XML document"
  (check-type doc doc)
  (let ((root (car (doc-elems-stack doc))))
    (check-type root (or null elem))
    root))


(defun get-doc-dtd (doc)
  (check-type doc doc)
  (doc-dtd doc))


(defun set-doc-xml-decl (doc xml-decl)
  (check-type doc doc)
  (check-type xml-decl xml-decl)
  (setf (doc-xml-decl doc) xml-decl))


(defun get-doc-xml-decl (doc)
  (check-type doc doc)
  (doc-xml-decl doc))
