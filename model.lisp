(in-package :model)


(defstruct uri
  (value "" :type string))

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
                  (close-by "")))
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

(defstruct (elem (:include node))
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
  (model "" :type string))

(defstruct (attr-decl (:include dtd-item))
  (elem-name "" :type string)
  (attr-name "" :type string)
  (type "" :type string)
  (default "" :type string))

(defstruct (nota-decl (:include dtd-item))
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))

(defstruct (int-ent-decl (:include dtd-item))
  (kind "" :type string)
  (name "" :type string)
  (value "" :type string))

(defstruct (ext-ent-decl (:include dtd-item))
  (kind "" :type string)
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))

(defstruct (unp-ent-decl (:include dtd-item))
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string))
  (nota-name "" :type string))

(defstruct (unp-int-subs (:include dtd-item))
  (content "" :type string))

(defstruct dtd
  (items nil :type list)
  (name "" :type string)
  (public-id nil :type (or null string))
  (system-id nil :type (or null string)))

(defstruct doc
  (xml-decl nil :type (or null xml-decl))
  (dtd nil :type (or null dtd))
  (elems-stack nil :type list))


;; TODO move these funcs to utils:

(defun non-empty-string-p (s)
  (and (stringp s) (string/= s "")))


(defun empty-string-to-nil (s)
  (check-type s string)
  (if (string= s "") nil s))

(defun try-as-string (s)
  (typecase s
    (null s)
    (string s)
    (keyword (symbol-name s))
    (t (format nil "~A" s))))

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
  (format stream "~A" uri))



(defun create-node (&key idx parent)
  (make-node :idx idx :parent parent))

(defun set-node-idx (node idx)
  (check-type node node)
  (assert (and (integerp idx) (>= idx 0)))
  (setf (node-idx node) idx))

(defun get-node-idx (node)
  (check-type node node)
  (node-idx node))

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
                                                   (cons (funcall non-elem-name n) dir)))
                         (collect-dir (node-parent n) dir)))
               (t dir))))
    (let* ((dir0 (collect-dir node nil))
           (dir1 (reverse dir0)))
      (if join-by
          (format nil (prep-join-fmt join-by) dir1)
          dir1))))

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




(defun create-attr (&key namespace-uri (local-name nil local-name-p) (qname nil qname-p) value specified)
  (assert (or (null namespace-uri) (non-empty-string-p namespace-uri)))
  (assert (or (null local-name) (non-empty-string-p local-name)))
  (assert (or (null qname) (non-empty-string-p qname)))
  (assert (or local-name-p qname-p))
  (make-attr :namespace-uri (when namespace-uri (create-uri namespace-uri))
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




(defun create-text (content)
  (assert (non-empty-string-p content))
  (make-text :content content))

(defun get-text-content (text)
  (check-type text text)
  (text-content text))




(defun create-pinstr (&key target data)
  (assert (non-empty-string-p target))
  (assert (or (null data) (non-empty-string-p data)))
  (make-pinstr :target target :data data))

(defun get-pinstr-target (pinstr)
  (check-type pinstr pinstr)
  (pinstr-target pinstr))

(defun get-pinstr-data (pinstr)
  (check-type pinstr pinstr)
  (pinstr-data pinstr))



(defun create-cdata (content)
  (assert (non-empty-string-p content))
  (make-cdata :content content))

(defun get-cdata-content (cdata)
  (check-type cdata cdata)
  (cdata-content cdata))




(defun create-comment (content)
  (assert (non-empty-string-p content))
  (make-comment :content content))

(defun get-comment-content (comment)
  (check-type comment comment)
  (comment-content comment))





(defun create-prefix-mappings (&optional items)
  (make-prefix-mappings :items items))

(defun add-prefix-mappings (prefix-mappings &rest new-pairs)
  (setf (prefix-mappings-items prefix-mappings)
        (append new-pairs
                (prefix-mappings-items prefix-mappings))))

(defun over-prefix-mappings (prefix-mappings &key (collect nil collect-p) (do nil do-p))
  (check-type prefix-mappings prefix-mappings)
  (assert (not (and collect-p do-p)))
  (cond (collect-p (mapcar collect
                           (prefix-mappings-items prefix-mappings)))
        (do-p      (dolist (pm (prefix-mappings-items prefix-mappings))
                     (funcall do pm)))
        (t (error "Pass either :COLLECT or :DO"))))





(defun create-elem (&key namespace-uri (local-name nil local-name-p) (qname nil qname-p)
                      prefix-mappings attributes children)
  (assert (or (null namespace-uri) (non-empty-string-p namespace-uri)))
  (assert (or (null local-name) (non-empty-string-p local-name)))
  (assert (or (null qname) (non-empty-string-p qname)))
  (assert (or local-name-p qname-p))
  (make-elem :namespace-uri (try-as-uri namespace-uri)
             :local-name local-name
             :qname qname
             :prefix-mappings prefix-mappings
             :attributes attributes
             :children children))

(defun get-elem-children-num (elem)
  (check-type elem elem)
  (length (elem-children elem)))

(defun over-elem-children (elem &key (collect nil collect-p) (do nil do-p))
  (check-type elem elem)
  (assert (not (and collect-p do-p)))
  (cond (collect-p (mapcar collect
                           (elem-children elem)))
        (do-p      (dolist (child (elem-children elem))
                     (funcall do child)))
        (t (error "Pass either :COLLECT or :DO"))))

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
      (over-elem-children elem :do #'defer-child-update)
      (dolist (deferred-update (reverse deferred-updates))
        (execute-deferred-update deferred-update))
      ;;(format t "         !!!!! AFTER: ~A~%" (mapcar #'node-idx (elem-children elem)))
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

(defun get-elem-attributes (elem)
  (check-type elem elem)
  (elem-attributes elem))

(defun get-elem-children (elem)
  (check-type elem elem)
  (elem-children elem))



(defun create-doctype (content)
  (assert (non-empty-string-p content))
  (make-doctype :content content))





(defun create-xml-decl (content)
  (assert (non-empty-string-p content))
  (make-xml-decl :content content))



(defun create-elem-decl (&key name model)
  (assert (non-empty-string-p name))
  (assert (or (consp model)
              (keywordp model)
              (non-empty-string-p model)))
  (make-elem-decl :name name :model (try-as-string model)))

(defun get-elem-decl-name (elem-decl)
  (check-type elem-decl elem-decl)
  (elem-decl-name elem-decl))

(defun get-elem-decl-model (elem-decl)
  (check-type elem-decl elem-decl)
  (elem-decl-model elem-decl))



(defun create-attr-decl (&key elem-name attr-name type default)
  (assert (non-empty-string-p elem-name))
  (assert (non-empty-string-p attr-name))
  (assert (or (keywordp type)
              (non-empty-string-p type)))
  (assert (or (keywordp default)
              (non-empty-string-p default)))
  (make-attr-decl :elem-name elem-name
                  :attr-name attr-name
                  :type (try-as-string type)
                  :default (try-as-string default)))

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


(defun create-nota-decl (&key name (public-id nil public-id-p) (system-id nil system-id-p))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-nota-decl :name name :public-id public-id :system-id system-id))

(defun get-nota-decl-name (nota-decl)
  (check-type nota-decl nota-decl)
  (nota-decl-name nota-decl))

(defun get-nota-decl-public-id (nota-decl)
  (check-type nota-decl nota-decl)
  (nota-decl-public-id nota-decl))

(defun get-nota-decl-system-id (nota-decl)
  (check-type nota-decl nota-decl)
  (nota-decl-system-id nota-decl))


(defun create-int-ent-decl (&key kind name value)
  (assert (or (keywordp kind)
              (non-empty-string-p kind)))
  (assert (non-empty-string-p name))
  (assert (non-empty-string-p value))
  (make-int-ent-decl :kind (try-as-string kind) :name name :value value))

(defun get-int-ent-decl-kind (int-ent-decl)
  (check-type int-ent-decl int-ent-decl)
  (int-ent-decl-kind int-ent-decl))

(defun get-int-ent-decl-name (int-ent-decl)
  (check-type int-ent-decl int-ent-decl)
  (int-ent-decl-name int-ent-decl))

(defun get-int-ent-decl-value (int-ent-decl)
  (check-type int-ent-decl int-ent-decl)
  (int-ent-decl-value int-ent-decl))



(defun create-ext-ent-decl (&key kind name (public-id nil public-id-p) (system-id nil system-id-p))
  (assert (or (keywordp kind)
              (non-empty-string-p kind)))
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (make-ext-ent-decl :kind (try-as-string kind) :name name :public-id public-id :system-id system-id))

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


(defun create-unp-ent-decl (&key name (public-id nil public-id-p) (system-id nil system-id-p) nota-name)
  (assert (non-empty-string-p name))
  (assert (or (null public-id) (non-empty-string-p public-id)))
  (assert (or (null system-id) (non-empty-string-p system-id)))
  (assert (or public-id-p system-id-p))
  (assert (non-empty-string-p nota-name))
  (make-unp-ent-decl :name name :public-id public-id :system-id system-id :nota-name nota-name))

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


(defun create-unp-int-subs (content)
  (assert (non-empty-string-p content))
  (make-unp-int-subs :content content))

(defun get-unp-int-subs-content (unp-int-subs)
  (check-type unp-int-subs unp-int-subs)
  (unp-int-subs-content unp-int-subs))


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
  (push item (dtd-items dtd)))

(defun get-dtd-items (dtd)
  (check-type dtd dtd)
  (dtd-items dtd))

(defun get-dtd-name (dtd)
  (check-type dtd dtd)
  (dtd-name dtd))

(defun get-dtd-public-id (dtd)
  (check-type dtd dtd)
  (dtd-public-id dtd))

(defun get-dtd-system-id (dtd)
  (check-type dtd dtd)
  (dtd-system-id dtd))


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
