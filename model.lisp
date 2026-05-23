(in-package :model)



(defun non-empty-string-p (s)
  (and (stringp s) (string/= s "")))



(defun empty-string-to-nil (s)
  (assert (stringp s))
  (if (string= s "") nil s))



(defstruct uri
  (value "" :type string))

(defun create-uri (uri-value)
  (assert (non-empty-string-p uri-value))
  (make-uri :value uri-value))



(defstruct node
  (idx nil :type (or null (integer 0)))
  (open-by "<" :type string)
  (close-by ">" :type string)
  (parent nil :type (or null node)))

(defun create-node (&key idx parent)
  (make-node :idx idx :parent parent))

(defun set-node-idx (node idx)
  (check-type node node)
  (assert (and (integerp idx) (>= idx 0)))
  (setf (node-idx node) idx))

(defun calc-node-dir (node &key with-idx non-elem-name join-by)
  "Collects DIR of a NODE (adding IDX, if WITH-IDX is T) of every ELEM en route and
returns the result as a list of strings. But if JOIN-BY was passed as some STRING,
then returns it as a STRING joining components by this delimiter"
  (check-type node node)
  (check-type join-by (or null string))
  (labels ((prep-idx (n)
             "Prepares IDX of a NODE N as a string"
             (write-to-string (node-idx n)))
           (prep-join-fmt (delim)
             "Prepares FORMAT string able to join items by DELIM"
             (concatenate 'string "~{~A~^" delim "~}"))
           (cons-idx-if (n lst)
             "Adds IDX of a NODE N to the front of list LST if WITH-IDX"
             (if with-idx
                 (cons (prep-idx n) lst)
                 lst))
           (collect-dir (n dir)
             "Recursively collects DIR from a node N to the top parent"
             (typecase n
               (null dir)
               (elem (collect-dir (node-parent n)
                                  (cons-idx-if n
                                               (cons (elem-local-name n) dir))))
               (node (if non-elem-name
                         (collect-dir (node-parent n)
                                      (cons-idx-if n
                                                   (cons (non-elem-name n) dir)))
                         (collect-dir (node-parent n) dir)))
               (t dir))))
    (let* ((dir0 (collect-dir node nil))
           (dir1 (reverse dir0)))
      (if join-by
          (format nil (prep-join-fmt join-by) dir1)
          dir1))))

(defun %add-child-node-to-elem (child-node to-elem)
  "Adds a CHILD-NODE to ELEM TO-ELEM"
  (assert child-node node)
  (assert to-elem elem)
  (symbol-macrolet ((elem-children (model:elem-children to-elem)))
    (setf elem-children (append elem-children (list child-node)))
    (setf (node-parent child-node) to-elem))
  (%numerate-elem-children to-elem))

(defun add-child-node-to-current-elem (child-node doc)
  "Adds CHILD-NODE to the current ELEM (tracked in DOC)"
  (assert child-node node)
  (assert doc doc)
  (let* ((elems-stack (model:doc-elems-stack doc))
         (cur-elem (car elems-stack)))
    (when cur-elem (%add-child-node-to-elem child-node cur-elem))))



(defstruct attr
  (namespace-uri nil :type (or null uri))
  (local-name nil :type (or null string))
  (qname nil :type (or null string))
  (value nil :type (or null string))
  (specified nil :type boolean))

(defun create-attr (&key namespace-uri (local-name nil local-name-p) (qname nil qname-p) value specified)
  (assert (or (null namespace-uri) (non-empty-string-p namespace-uri)))
  (assert (or (null local-name) (non-empty-string-p local-name)))
  (assert (or (null qname) (non-empty-string-p qname)))
  (assert (or local-name-p qname-p))
  (make-attr :namespace-uri namespace-uri
             :local-name local-name
             :qname qname
             :value value
             :specified specified))



(defstruct (text (:include node))
  (open-by "")
  (close-by "")
  (content "" :type string))

(defun create-text (content)
  (assert (non-empty-string-p content))
  (make-text :content content))



(defstruct (pinstr (:include node))
  (open-by "<?")
  (close-by "?>")
  (target "" :type string)
  (data nil :type (or null string)))

(defun create-pinstr (&key target data)
  (assert (non-empty-string-p target))
  (assert (or (null data) (non-empty-string-p data)))
  (make-pinstr :target target :data data))



(defstruct (cdata (:include node))
  (open-by "<![CDATA[")
  (close-by "]]>")
  (content "" :type string))

(defun create-cdata (content)
  (assert (non-empty-string-p content))
  (make-cdata :content content))



(defstruct (comment (:include node))
  (open-by "<!--")
  (close-by "-->")
  (content "" :type string))

(defun create-comment (content)
  (assert (non-empty-string-p content))
  (make-comment :content content))



(defstruct prefix-mappings
  (items nil :type list))

(defun create-prefix-mappings (&optional items)
  (make-prefix-mappings :items items))

(defun add-prefix-mappings (prefix-mappings &rest new-pairs)
  (setf (prefix-mappings-items prefix-mappings)
        (append new-pairs
                (prefix-mappings-items prefix-mappings))))



(defstruct (elem (:include node))
  (namespace-uri nil :type (or null uri))
  (local-name nil :type (or null string))
  (qname nil :type (or null string))
  (prefix-mappings nil :type (or null prefix-mappings))
  (attributes nil :type list)
  (children nil :type list))

(defun create-elem (&key namespace-uri (local-name nil local-name-p) (qname nil qname-p)
                      prefix-mappings attributes children)
  (assert (or (null namespace-uri) (non-empty-string-p namespace-uri)))
  (assert (or (null local-name) (non-empty-string-p local-name)))
  (assert (or (null qname) (non-empty-string-p qname)))
  (assert (or local-name-p qname-p))
  (make-elem :namespace-uri namespace-uri
             :local-name local-name
             :qname qname
             :prefix-mappings prefix-mappings
             :attributes attributes
             :children children))

(defun elem-children-num (elem)
  (check-type elem elem)
  (length (elem-children elem)))

(defun over-elem-children (elem &key (collect nil collect-p) (do nil do-p))
  (check-type elem elem)
  (assert (not (and collect-p do-p)))
  (cond (collect-p (mapcar collect
                           (elem-children elem)))
        (do-p      (dolist (child (elem-children elem))
                     (funcall do child)))
        (t (error "Pass either :collect or :do")))))

(defun %numerate-elem-children (elem)
  "Refreshes NODE-IDX field of ....."
  (check-type elem elem)
  (let ((counters nil)
        (deferred-updates nil))
    (labels ((calc-child-id (child)
               (format nil "~A--~A" (type-of child) (calc-node-dir child :join-by "")))
             (defer-child-update (child)
               (when (typep child 'node)
                 (let* ((child-id (calc-child-id child))
                        (child-counter (assoc child-id counters :test #'equal))
                        (child-num (or (cdr child-counter) 0)))
                   ;; (format t "!!!!!!!!!!!!!!!!!!!!! ~A  ~A (~A): ~A~%"
                   ;;         (type-of child) (node-dir child) child-id child-num)
                   ;; (setf (node-idx child) child-num)
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
      (over-elem-children elem :do defer-child-update)
      (dolist (deferred-update (reverse deferred-updates))
        (execute-deferred-update deferred-update))
      ;;(format t "         !!!!! AFTER: ~A~%" (mapcar #'node-idx (elem-children elem)))
      )))

(defun enter-elem (elem doc)
  "Pushes ELEM to ELEMS-STACK making it the current element"
  (check-type elem elem)
  (check-type doc doc)
    (push elem (model:doc-elems-stack doc)))

(defun exit-from-elem (doc)
  "Pops (like Linux popd(1) command) current ELEM from the stack of XML elements"
  (symbol-macrolet ((elems-stack (model:doc-elems-stack doc)))
    (when (cdr elems-stack)
      (pop elems-stack))))



(defstruct doctype
  (content "" :type string))

(defun create-doctype (content)
  (assert (non-empty-string-p content))
  (make-doctype :content content))



(defstruct xml-decl
  (content "" :type string))

(defun create-xml-decl (content)
  (assert (non-empty-string-p content))
  (make-xml-decl :content content))



(defstruct dtd-item)


(defstruct (elem-decl (:include dtd-item))
  (name "" :type string)
  (model "" :type string))

(defun create-elem-decl (&key name model)
  (assert (non-empty-string-p name))
  (assert (non-empty-string-p model))
  (make-elem-decl :name name :model model))



(defstruct (attr-decl (:include dtd-item))
  (elem-name "" :type string)
  (attr-name "" :type string)
  (type "" :type string)
  (default "" :type string))

(defun create-attr-decl (&key elem-name attr-name type default)
  (assert (non-empty-string-p elem-name))
  (assert (non-empty-string-p attr-name))
  (assert (non-empty-string-p type))
  (assert (non-empty-string-p default))
  (make-attr-decl :elem-name elem-name :attr-name attr-name :type type :default default))



(defstruct (nota-decl (:include dtd-item))
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))

(defun create-nota-decl (&key name (public-id nil public-id-p) (system-id nil system-id-p))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-nota-decl :name name :public-id public-id :system-id system-id))



(defstruct (int-ent-decl (:include dtd-item))
  (kind "" :type string)
  (name "" :type string)
  (value "" :type string))

(defun create-int-ent-decl (&key kind name value)
  (assert (non-empty-string-p kind))
  (assert (non-empty-string-p name))
  (assert (non-empty-string-p value))
  (make-int-ent-decl :kind kind :name name :value value))



(defstruct (ext-ent-decl (:include dtd-item))
  (kind "" :type string)
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))

(defun create-ext-ent-decl (&key kind name (public-id nil public-id-p) (system-id nil system-id-p))
  (assert (non-empty-string-p kind))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-int-ent-decl :kind kind :name name :public-id public-id :system-id system-id))



(defstruct (unp-ent-decl (:include dtd-item))
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string))
  (nota-name "" :type string))

(defun create-unp-ent-decl (&key name (public-id nil public-id-p) (system-id nil system-id-p) nota-name)
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (assert (non-empty-string-p nota-name))
  (make-unp-ent-decl :name name :public-id public-id :system-id system-id :nota-name nota-name))



(defstruct (unp-int-subs (:include dtd-item))
  (content "" :type string))

(defun create-unp-int-subs (content)
  (assert (non-empty-string-p content))
  (make-unp-int-subs :content content))



(defstruct dtd
  (items nil :type list)
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))

(defun create-dtd (&key items name (public-id nil public-id-p) (system-id nil system-id-p))
  (assert (every (lambda (it) (dtd-item-p it)) items))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-dtd :items items :name name :public-id public-id :system-id system-id))

(defun add-dtd-item (dtd item)
  (check-type dtd dtd)
  (check-type item dtd-item)
  (pushf item (dtd-items dtd)))



(defstruct doc
  (xml-decl nil :type (or null xml-decl))
  (dtd nil :type (or null dtd))
  (elems-stack nil :type list))

(defun set-doc-dtd (doc dtd)
  (check-type doc doc)
  (check-type dtd dtd)
  (setf (doc-dtd doc) dtd))

;; (defclass uri ()
;;   ((value :type string
;;           :documentation ""
;;           :accessor uri-value)))


;; (defclass ns ()
;;   ((uri :type uri
;;         :documentation ""
;;         :accessor ns-uri)))


;; (defclass prefix ()
;;   ((ns :initform nil :type ns
;;        :documentation ""
;;        :accessor prefix-ns)))


;; (defclass local-name ()
;;   ((value :type string
;;           :documentation ""
;;           :accessor local-name-value)))


;; (defclass name ()
;;   ((local-name :type local-name
;;                :documentation ""
;;                :accessor name-local-name)
;;    (prefix :initform nil
;;            :type prefix
;;            :documentation ""
;;            :accessor name-prefix)))


;; (defclass dir ()
;;   ((names :type cons
;;           :documentation "Non-empty list of NAME"
;;           :accessor dir-names)))

;; (defun path-depth (dir)
;;   (length (dir-names dir)))


;; (defclass node ()
;;   ((dir :type dir ;; FIXME it seems I use it as string!!!
;;         :documentation "A construct is located at some DIR reflecting it nesting"
;;         :accessor node-dir)
;;    (idx :type integer
;;         :documentation "Index of a node among same type siblings"
;;         :initform -1
;;         :accessor node-idx)
;;    (open-by :type string
;;             :initform "<"
;;             :reader node-open-by
;;             :documentation "")
;;    (close-by :type string
;;              :initform ">"
;;              :reader node-close-by
;;              :documentation "")))


;; (defclass attr ()
;;   ((namespace-uri :type string
;;                   :documentation ""
;;                   :initform ""
;;                   :initarg :namespace-uri
;;                   :accessor attr-namespace-uri)
;;    (local-name :type string
;;                :documentation ""
;;                :initform ""
;;                :initarg :local-name
;;                :accessor attr-local-name)
;;    (qname :type string  ;; TODO start to use my NAME class
;;           :documentation ""
;;           :initform ""
;;           :initarg :qname
;;           :accessor attr-qname)
;;    (value :type string
;;           :documentation ""
;;           :initform ""
;;           :initarg :value
;;           :accessor attr-value)
;;    (specified :type boolean
;;               :documentation ""
;;               :initform nil
;;               :initarg :specified
;;               :accessor attr-specified)))


;; (defclass text (node)
;;   ((open-by :initform "" :reader text-open-by)
;;    (close-by :initform "" :reader text-close-by)
;;    (content :type string
;;             :initarg :content
;;             :documentation ""
;;             :accessor text-content)))


;; (defclass pinstr (node)
;;   ((open-by :initform "<?" :reader pinstr-open-by)
;;    (close-by :initform "?>" :reader pinstr-close-by)
;;    (target :type string
;;            :initform ""
;;            :initarg :target
;;            :accessor pinstr-target
;;            :documentation "")
;;    (content :type string
;;             :documentation ""
;;             :initarg :content
;;             :accessor pinstr-content))
;;   (:documentation "Processing instruction"))


;; (defclass cdata (node)
;;   ((open-by :initform "<![CDATA[" :reader cdata-open-by)
;;    (close-by :initform "]]>" :reader cdata-close-by)
;;    (content :type string
;;             :documentation ""
;;             :initform ""
;;             :initarg :content
;;             :accessor cdata-content)))


;; (defclass comment (node)
;;   ((open-by :initform "<!--")
;;    (close-by :initform "-->")
;;    (content :type string
;;             :initarg :content
;;             :documentation ""
;;             :accessor comment-content)))


;; (defclass empty (node)  ;; FIXME wtf is it?
;;   ((attributes :type list
;;                :initform nil
;;                :documentation ""
;;                :accessor empty-attributes)
;;    (name :type name
;;          :documentation ""
;;          :accessor empty-name)))


;; (defclass prefix-mappings ()
;;   ((items :type list
;;           :initform nil
;;           :initarg :items
;;           :accessor prefix-mappings-items
;;           :documentation "Pairs (list of cons)")))


;; (defun add-prefix-mappings (prefix-mappings &rest new-pairs)
;;   (setf (prefix-mappings-items prefix-mappings)
;;         (append new-pairs
;;                 (prefix-mappings-items prefix-mappings))))


;; (defclass elem (node)
;;   ((namespace-uri :type string
;;                   :initform ""
;;                   :initarg :namespace-uri
;;                   :documentation ""
;;                   :accessor elem-namespace-uri)
;;    (local-name :type string
;;                :initform ""
;;                :initarg :local-name
;;                :documentation ""
;;                :accessor elem-local-name)
;;    (qname :type string
;;           :initform ""
;;           :initarg :qname
;;           :documentation ""
;;           :accessor elem-qname)
;;    (prefix-mappings :type list
;;                     :documentation ""
;;                     :initform nil
;;                     :initarg :prefix-mappings
;;                     :accessor elem-prefix-mappings)
;;    (attributes :type list
;;                :initform nil
;;                :initarg :attributes
;;                :documentation ""
;;                :accessor elem-attributes)
;;    (children :type list
;;              :documentation ""
;;              :initform nil
;;              :accessor elem-children)))

;; (defun elem-children-num (elem)
;;   (length (elem-children elem)))


;; (defclass entity (node) ; TODO do I use it?
;;   ((open-by :initform "&" :reader entity-open-by)
;;    (close-by :initform ";" :reader entity-close-by)
;;    (entity-code :type string
;;                 :documentation ""
;;                 :accessor entity-entity-code)))


;; (defclass doctype ()
;;   ((content :type string
;;             :documentation ""
;;             :accessor doctype-content
;;             :initarg :content)))


;; (defclass xml-decl ()
;;   ((content :type string
;;             :documentation ""
;;             :accessor xml-decl-content
;;             :initarg :content)))


;; (defclass elem-decl ()
;;   ((name :type string
;;          :documentation ""
;;          :accessor elem-decl-name
;;          :initarg :name)
;;    (model :type string
;;           :documentation ""
;;           :accessor elem-decl-model
;;           :initarg :model)))


;; (defclass attr-decl ()
;;   ((element-name :type string
;;                  :documentation ""
;;                  :accessor attr-decl-element-name
;;                  :initarg :element-name)
;;    (attribute-name :type string
;;                    :documentation ""
;;                    :accessor attr-decl-attribute-name
;;                    :initarg :attribute-name)
;;    (type :type string
;;          :documentation ""
;;          :accessor attr-decl-type
;;          :initarg :type)
;;    (default :type string
;;             :documentation ""
;;             :accessor attr-decl-default
;;             :initarg :default)))


;; (defclass nota-decl ()
;;   ((name :type string
;;          :documentation ""
;;          :accessor nota-decl-name
;;          :initform ""
;;          :initarg :name)
;;    (public-id :type string
;;               :documentation ""
;;               :accessor nota-decl-public-id
;;               :initform ""
;;               :initarg :public-id)
;;    (system-id :type string
;;               :documentation ""
;;               :accessor nota-decl-system-id
;;               :initform ""
;;               :initarg :system-id)))


;; (defclass int-ent-decl ()
;;   ((kind :type string
;;          :documentation ""
;;          :accessor int-ent-decl-kind
;;          :initform ""
;;          :initarg :kind)
;;    (name :type string
;;          :documentation ""
;;          :accessor int-ent-decl-name
;;          :initform ""
;;          :initarg :name)
;;    (value :type string
;;           :documentation ""
;;           :accessor int-ent-decl-value
;;           :initform ""
;;           :initarg :value)))


;; (defclass ext-ent-decl ()
;;   ((kind :type string
;;          :documentation ""
;;          :accessor ext-ent-decl-kind
;;          :initform ""
;;          :initarg :kind)
;;    (name :type string
;;          :documentation ""
;;          :accessor ext-ent-decl-name
;;          :initform ""
;;          :initarg :name)
;;    (public-id :type string
;;               :documentation ""
;;               :accessor ext-ent-decl-public-id
;;               :initform ""
;;               :initarg :public-id)
;;    (system-id :type string
;;               :documentation ""
;;               :accessor ext-ent-decl-system-id
;;               :initform ""
;;               :initarg :system-id)))


;; (defclass unp-ent-decl ()
;;   ((name :type string
;;          :documentation ""
;;          :accessor unp-ent-decl-name
;;          :initform ""
;;          :initarg :name)
;;    (public-id :type string
;;               :documentation ""
;;               :accessor unp-ent-decl-public-id
;;               :initform ""
;;               :initarg :public-id)
;;    (system-id :type string
;;               :documentation ""
;;               :accessor unp-ent-decl-system-id
;;               :initform ""
;;               :initarg :system-id)
;;    (nota-name :type string
;;               :documentation ""
;;               :accessor unp-ent-decl-nota-name
;;               :initform ""
;;               :initarg :nota-name)))


;; (defclass unp-int-subs ()
;;   ((content :type string
;;             :documentation ""
;;             :accessor unp-int-subs-content
;;             :initform ""
;;             :initarg :content)))


;; (defclass dtd ()
;;   ((items :type list  ;; items as attr-decl, elem-decl...
;;           :documentation ""
;;           :accessor dtd-items
;;           :initform nil
;;           :initarg :items)
;;    (name :type string
;;          :documentation ""
;;          :accessor dtd-name
;;          :initform ""
;;          :initarg :name)
;;    (public-id :type string
;;               :documentation ""
;;               :accessor dtd-public-id
;;               :initform ""
;;               :initarg :public-id)
;;    (system-id :type string
;;               :documentation ""
;;               :accessor dtd-system-id
;;               :initform ""
;;               :initarg :system-id)))


;; (defclass doc ()
;;   ((xml-decl :accessor doc-xml-decl ;; TODO find a way to populate it
;;              :initform nil
;;              :initarg :xml-decl)
;;    (dtd :type dtd
;;         :accessor doc-dtd
;;         :documentation ""
;;         :initform nil)
;;    (elems-stack :type list  ;; the last elem is the root
;;                 :documentation ""
;;                 :initform nil
;;                 :accessor doc-elems-stack)))
<
