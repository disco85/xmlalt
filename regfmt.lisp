(in-package :regfmt)
;; TODO maybe to unite multiple lines of some constructs to 1 line?

(defparameter *is* " = ")
(defparameter *sep* "::")
(defparameter +whitespace+ '(#\Space #\Tab #\Newline #\Return))
(defparameter *stdfmt* "~/regfmt:key/~/regfmt:esc/~%")

(defun dyn-escape-chars ()
  (coerce (remove-if (lambda (c) (member c +whitespace+))
                     (remove-duplicates
                      (concatenate 'string *is* *sep*)
                      :test #'char=))
          'list))


(defun as-internal-str (s)
  "Everything treating as internal thing (indexes, escaping, etc)
is shown in this way"
  (format nil "<~A>" s))


(defun safe-key-string (str)
  (let ((dyn-extra (dyn-escape-chars)))
    (flet ((safer (c)
             (cond
               ((char= c #\<) (as-internal-str "<"))
               ((char= c #\>) (as-internal-str ">"))
               ((char= c #\Newline) (as-internal-str "NL"))
               ((member c dyn-extra :test #'char=)
                (as-internal-str c))   ; escape dynamic chars
               (t (string c)))))
      (apply #'concatenate 'string (map 'list #'safer str)))))


(defun to-safe-str (obj)
  "Converts OBJ to STRING and make it safe on the sense of SAFE-KEY-STRING"
  (let* ((s0 (princ-to-string obj))
         (s1 (safe-key-string s0)))
    s1))


(defun esc (stream arg &rest args)
  "Can be used in FORMAT: (format t \"Start ~/regfmt:esc/ End\" \"aa/bb\")
to get similar to ~A but with escaping"
  (declare (ignore args))
  (write-string (to-safe-str arg) stream))


(defun key (stream arg &rest args)
  "ARG is a list of keys"
  (declare (ignore args))
  (flet ((escape-1-char-word (w)
           (if (= 1 (length w)) ;; FIXME not 1 char word, but when symbol is 1 len ":".
               (to-safe-str w)  ;; FIXME or don't esc "<..>" if len is > 3?
               w)))
    (let ((fmtstr (format nil "~A~A~A~A" "~{~A~^" *sep* "~}" *is*))
          (arg1 (remove nil arg)))
      (format stream fmtstr (mapcar #'escape-1-char-word arg1)))))


(defun node-idx-to-str (node)
  "If (NODE-IDX NODE) is NIL returns NIL, else it returns a STRING"
  (check-type node model:node)
  (let ((idx (model:get-node-idx node)))
    (when idx (as-internal-str idx))))


(defun serialize (doc stream)
  "Serializes MODEL:DOC object"
  (serialize-dtd doc stream)
  (with-truly root (model:get-doc-root doc)
    (serialize-nodes doc root stream)))


(defun serialize-dtd (doc stream)
  (with-truly dtd (model:get-doc-dtd doc)
    (serialize-dtd-attrs doc dtd stream)
    (with-truly items (model:get-dtd-items dtd)
      (serialize-dtd-items doc dtd items stream))))


(defun serialize-dtd-attrs (doc dtd stream)
  (declare (ignore doc))
  (with-truly name (model:get-dtd-name dtd)
    (format stream *stdfmt* '("<dtd>" "<name>") name))
  (with-truly public-id (model:get-dtd-public-id dtd)
    (format stream *stdfmt* '("<dtd>" "<public-id>") public-id))
  (with-truly system-id (model:get-dtd-system-id dtd)
    (format stream *stdfmt* '("<dtd>" "<system-id>") system-id)))


(defun serialize-dtd-items (doc dtd items stream)
  (dolist (item items)
    (when item (serialize-dtd-item doc dtd item stream))))


(defun serialize-dtd-item (doc dtd item stream)
  (declare (ignore doc dtd))
  (typecase item
    (model:attr-decl
     (with-truly element-name (model:get-attr-decl-elem-name item)
       (format stream *stdfmt* '("<dtd>" "<attribute>" "<element-name>") element-name))
     (with-truly attribute-name (model:get-attr-decl-attr-name item)
       (format stream *stdfmt* '("<dtd>" "<attribute>" "<attribute-name>") attribute-name))
     (with-truly type (model:get-attr-decl-type item)
       (format stream *stdfmt* '("<dtd>" "<attribute>" "<type>") type))
     (with-truly default (model:get-attr-decl-default item)
       (format stream *stdfmt* '("<dtd>" "<attribute>" "<default>") default)))

    (model:elem-decl
     (with-truly name (model:get-elem-decl-name item)
       (format stream *stdfmt* '("<dtd>" "<element>" "<name>") name))
     (with-truly model (model:get-elem-decl-model item)
       (format stream *stdfmt* '("<dtd>" "<element>" "<model>") model)))

    (model:nota-decl
     (with-truly name (model:get-nota-decl-name item)
       (format stream *stdfmt* '("<dtd>" "<notation>" "<name>") name))
     (with-truly public-id (model:get-nota-decl-public-id item)
       (format stream *stdfmt* '("<dtd>" "<notation>" "<public-id>") public-id))
     (with-truly system-id (model:get-nota-decl-system-id item)
       (format stream *stdfmt* '("<dtd>" "<notation>" "<system-id>") system-id)))

    (model:int-ent-decl
     (with-truly kind (model:get-int-ent-decl-kind item)
       (format stream *stdfmt* '("<dtd>" "<internal-entity>" "<kind>") kind))
     (with-truly name (model:get-int-ent-decl-name item)
       (format stream *stdfmt* '("<dtd>" "<internal-entity>" "<name>") name))
     (with-truly value (model:get-int-ent-decl-value item)
       (format stream *stdfmt* '("<dtd>" "<internal-entity>" "<value>") value)))

    (model:ext-ent-decl
     (with-truly kind (model:get-ext-ent-decl-kind item)
       (format stream *stdfmt* '("<dtd>" "<external-entity>" "<kind>") kind))
     (with-truly name (model:get-ext-ent-decl-name item)
       (format stream *stdfmt* '("<dtd>" "<external-entity>" "<name>") name))
     (with-truly public-id (model:get-ext-ent-decl-public-id item)
       (format stream *stdfmt* '("<dtd>" "<external-entity>" "<public-id>") public-id))
     (with-truly system-id (model:get-ext-ent-decl-system-id item)
       (format stream *stdfmt* '("<dtd>" "<external-entity>" "<system-id>") system-id)))

    (model:unp-ent-decl
     (with-truly name (model:get-unp-ent-decl-name item)
       (format stream *stdfmt* '("<dtd>" "<unparsed-entity>" "<name>") name))
     (with-truly public-id (model:get-unp-ent-decl-public-id item)
       (format stream *stdfmt* '("<dtd>" "<unparsed-entity>" "<public-id>") public-id))
     (with-truly system-id (model:get-unp-ent-decl-system-id item)
       (format stream *stdfmt* '("<dtd>" "<unparsed-entity>" "<system-id>") system-id))
     (with-truly nota-name (model:get-unp-ent-decl-nota-name item)
       (format stream *stdfmt* '("<dtd>" "<unparsed-entity>" "<notation-name>") nota-name)))

    (model:unp-int-subs
     (with-truly content (model:get-unp-int-subs-content item)
       (format stream *stdfmt* '("<dtd>" "<unparsed-internal-subset>") content)))))


(defun serialize-elem-attributes (doc node attributes stream)
  (declare (ignore doc))
  (dolist (attribute attributes)
    (format stream *stdfmt*
            (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                  (node-idx-to-str node)
                  (concatenate 'string "@" (model:get-attr-qname attribute)))
            (model:get-attr-value attribute))
    (with-truly local-name (model:get-attr-local-name attribute)
      (format stream *stdfmt*
              (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                    (node-idx-to-str node)
                    (concatenate 'string "@" (model:get-attr-qname attribute))
                    "<local-name>")
              local-name))
    (with-truly namespace-uri (model:get-attr-namespace-uri attribute)
      (format stream *stdfmt*
              (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                    (node-idx-to-str node)
                    (concatenate 'string "@" (model:get-attr-qname attribute))
                    "<namespace-uri>")
              (model:write-uri namespace-uri)))))


(defun serialize-prefix-mappings (doc node prefix-mappings stream)
  (declare (ignore doc))
  (model:over-prefix-mappings
   prefix-mappings
   :do (lambda (pair)
         (format stream *stdfmt*
                 (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                       (node-idx-to-str node)
                       "<prefix-mapping>"
                       (car pair))
                 (cdr pair)))))

;; TODO root::@xmlns:x = urn<:>x  -- but in key : is not escaped (see above why)
;; TODO some elems can be skipped due to with-truly - is it ok for such?
;; TODO no Book1 - text inside <book>..


(defun serialize-nodes (doc node stream)
  "Serializes XML node"
  (typecase node
    (model:elem
     (if (> (model:get-elem-children-num node) 0)
         (format stream *stdfmt*
                 (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                       (node-idx-to-str node)
                       "<children>")
                 (model:get-elem-children-num node)))
     ;; (with-truly local-name (model:elem-local-name node)
     ;;   (format stream *stdfmt*
     ;;           (list (model:node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
     ;;                 "<local-name>")
     ;;           local-name))
     (with-truly namespace-uri (model:get-elem-namespace-uri node)
       (format stream *stdfmt*
               (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                     (node-idx-to-str node)
                     "<namespace-uri>")
               (model:write-uri namespace-uri)))
     (with-truly qname (model:get-elem-qname node)
       (format stream *stdfmt*
               (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                     (node-idx-to-str node)
                     "<qname>")
               qname))
     (with-truly prefix-mappings (model:get-elem-prefix-mappings node)
       (serialize-prefix-mappings doc node prefix-mappings stream))
     (with-truly attributes (model:get-elem-attributes node)
       (serialize-elem-attributes doc node attributes stream))
     (model:over-elem-children node
       :do (lambda (child) (serialize-nodes doc child stream))))

    (model:text
     (with-truly content (model:get-text-content node)
       (format stream *stdfmt*
               (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                     (node-idx-to-str node)
                     "<text>")
               content)))

    (model:comment
     (with-truly content (model:get-comment-content node)
       (format stream *stdfmt*
               (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                     (node-idx-to-str node)
                     "<comment>")
               content)))

    (model:cdata
     (with-truly content (model:get-cdata-content node)
       (format stream *stdfmt*
               (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                     (node-idx-to-str node)
                     "<cdata>")
               content)))

    (model:pinstr
     (with-truly data (model:get-pinstr-data node)
       (format stream *stdfmt*
               (list (model:calc-node-dir node :with-idx-as #'as-internal-str :join-by *sep*)
                     (node-idx-to-str node)
                     "<processing-instruction>"
                     (model:get-pinstr-target node))
               data)))))
