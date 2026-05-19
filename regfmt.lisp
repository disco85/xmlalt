(in-package :regfmt)
;; TODO maybe to unite multiple lines of some constructs to 1 line?

(defparameter *is* " = ")
(defparameter *sep* "\\")
(defparameter +whitespace+ '(#\Space #\Tab #\Newline #\Return))

(defun dyn-escape-chars ()
  (coerce (remove-if (lambda (c) (member c +whitespace+))
                     (remove-duplicates
                      (concatenate 'string *is* *sep*)
                      :test #'char=))
          'list))


(defun safe-key-string (str)
  (let ((dyn-extra (dyn-escape-chars)))
    (flet ((safer (c)
             (cond
               ((char= c #\<) "<<>")
               ((char= c #\>) "<>>")
               ((char= c #\Newline) "<NL>")
               ((member c dyn-extra :test #'char=)
                (format nil "<~A>" c))   ; escape dynamic chars
               (t (string c)))))
      (apply #'concatenate 'string (map 'list #'safer str)))))


(defun to-safe-str (obj)
  "Converts OBJ to STRING and make it safe on the sense of SAFE-KEY-STRING"
  (let* ((s0 (princ-to-string obj))
         (s1 (safe-key-string s0)))
    s1))

(defun esc (stream arg &rest args)
  "Can be used in FORMAT: (format t \"Start ~/regfmt:esc/ End\" \"aa/bb\") to get
similar to ~A but with escaping"
  (declare (ignore args))
  (write-string (to-safe-str arg) stream))

(defun key (stream arg &rest args)
  (declare (ignore args))
  (let ((fmtstr (format nil "~A~A~A~A" "~{~A~^" *sep* "~}" *is*)))
    (format stream fmtstr arg)))

(defun serialize (doc stream)
  "Serializes MODEL:DOC object"
  (serialize-dtd doc stream)
  (with-truly root (car (model:doc-elems-stack doc))
    (serialize-nodes doc root stream)))


(defun serialize-dtd (doc stream)
  (with-truly dtd (model:doc-dtd doc)
    (serialize-dtd-attrs doc dtd stream)
    (with-truly items (model:dtd-items dtd)
      (serialize-dtd-items doc dtd items stream))))


(defun serialize-dtd-attrs (doc dtd stream)
  (declare (ignore doc))
  (with-truly name (model:dtd-name dtd)
    (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<name>") name))
  (with-truly public-id (model:dtd-public-id dtd)
    (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<public-id>") public-id))
  (with-truly system-id (model:dtd-system-id dtd)
    (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<system-id>") system-id)))


(defun serialize-dtd-items (doc dtd items stream)
  (dolist (item items)
    (when item (serialize-dtd-item doc dtd item stream))))


(defun serialize-dtd-item (doc dtd item stream)
  (declare (ignore doc dtd))
  (typecase item
    (model:attr-decl
     (with-truly element-name (model:attr-decl-element-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<attribute>" "<element-name>") element-name))
     (with-truly attribute-name (model:attr-decl-attribute-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<attribute>" "<attribute-name>") attribute-name))
     (with-truly type (model:attr-decl-type item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<attribute>" "<type>") type))
     (with-truly default (model:attr-decl-default item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<attribute>" "<default>") default)))

    (model:elem-decl
     (with-truly name (model:elem-decl-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<element>" "<name>") name))
     (with-truly model (model:elem-decl-model item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<element>" "<model>") model)))

    (model:nota-decl
     (with-truly name (model:nota-decl-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<notation>" "<name>") name))
     (with-truly public-id (model:nota-decl-public-id item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<notation>" "<public-id>") public-id))
     (with-truly system-id (model:nota-decl-system-id item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<notation>" "<system-id>") system-id)))

    (model:int-ent-decl
     (with-truly kind (model:int-ent-decl-kind item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<internal-entity>" "<kind>") kind))
     (with-truly name (model:int-ent-decl-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<internal-entity>" "<name>") name))
     (with-truly value (model:int-ent-decl-value item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<internal-entity>" "<value>") value)))

    (model:ext-ent-decl
     (with-truly kind (model:ext-ent-decl-kind item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<external-entity>" "<kind>") kind))
     (with-truly name (model:ext-ent-decl-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<external-entity>" "<name>") name))
     (with-truly public-id (model:ext-ent-decl-public-id item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<external-entity>" "<public-id>") public-id))
     (with-truly system-id (model:ext-ent-decl-system-id item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<external-entity>" "<system-id>") system-id)))

    (model:unp-ent-decl
     (with-truly name (model:unp-ent-decl-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<unparsed-entity>" "<name>") name))
     (with-truly public-id (model:unp-ent-decl-public-id item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<unparsed-entity>" "<public-id>") public-id))
     (with-truly system-id (model:unp-ent-decl-system-id item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<unparsed-entity>" "<system-id>") system-id))
     (with-truly nota-name (model:unp-ent-decl-nota-name item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<unparsed-entity>" "<notation-name>") nota-name)))

    (model:unp-int-subs
     (with-truly content (model:unp-int-subs-content item)
       (format stream "~/regfmt:key/~/regfmt:esc/~%" '("<dtd>" "<unparsed-internal-subset>") content)))))


(defun serialize-elem-attributes (doc node attributes stream)
  (declare (ignore doc))
  (dolist (attribute attributes)
    (format stream "~/regfmt:key/~/regfmt:esc/~%"
            (list (utils:subs (model:node-dir node) "/" *sep*)
                  (concatenate 'string "@" (model:attr-local-name attribute)))
            (model:attr-value attribute))
    (with-truly qname (model:attr-qname attribute)
      (format stream "~/regfmt:key/~/regfmt:esc/~%"
            (list (utils:subs (model:node-dir node) "/" *sep*)
                  (concatenate 'string "@" (model:attr-local-name attribute))
                  "<qname>")
            qname))
    (with-truly namespace-uri (model:attr-namespace-uri attribute)
      (format stream "~/regfmt:key/~/regfmt:esc/~%"
            (list (utils:subs (model:node-dir node) "/" *sep*)
                  (concatenate 'string "@" (model:attr-local-name attribute))
                  "<namespace-uri>")
              namespace-uri))))


(defun serialize-prefix-mappings (doc node prefix-mappings stream)
  (declare (ignore doc node))
  (dolist (pair (model:prefix-mappings-items prefix-mappings))
    (format stream ".EL.PREF.MAP ~A ~A~%"
            (car pair)
            (cdr pair))))


(defun serialize-nodes (doc node stream)
  "Serializes XML node"
  (typecase node
    (model:elem
     (format stream ".EL ~A ~A~%" (model:elem-children-num node) (model:node-dir node))
     (with-truly namespace-uri (model:elem-namespace-uri node)
       (format stream ".EL.NS ~A~%" namespace-uri))
     (with-truly local-name (model:elem-local-name node)
       (format stream ".EL.LOC.NAME ~A~%" local-name))
     (with-truly qname (model:elem-qname node)
       (format stream ".EL.Q.NAME ~A~%" qname))
     (with-truly prefix-mappings (model:elem-prefix-mappings node)
       (serialize-prefix-mappings doc node prefix-mappings stream))
     (with-truly attributes (model:elem-attributes node)
       (serialize-elem-attributes doc node attributes stream))
     (with-truly children (model:elem-children node)
       (dolist (child children)
         (serialize-nodes doc child stream)))
     (format stream ".ELE ~A~%" (model:node-dir node)))

    (model:text
     (with-truly content (model:text-content node)
       (format stream ".TEXT~%~A~%.TEXTE~%" content)))

    (model:comment
     (with-truly content (model:comment-content node)
       (format stream ".COM~%~A~%.COME~%" content)))

    (model:cdata
     (with-truly content (model:cdata-content node)
       (format stream ".CD~%~A~%.CDE~%" content)))

    (model:pinstr
     (with-truly target (model:pinstr-target node)
       (format stream ".PI.TARG ~A~%" target))
     (with-truly content (model:pinstr-content node)
       (format stream ".PI~%~A~%.PIE~%" content)))))
