(in-package :regfmt)

;; TODO maybe to unite multiple lines of some constructs to 1 line?

(defun safe-key-string (str)
  (flet ((safer (c)
           (case c
             (#\< "<<>")
             (#\> "<>>")
             (#\/ "</>")
             (#\Newline "<NL>")
             (t (string c)))))
    (apply #'concatenate 'string (map 'list #'safer str))))

(defun esc (stream arg &rest args)
  "Can be used in FORMAT: (format t \"Start ~/esc/ End\" \"aa/bb\") to get
similar to ~A but with escaping"
  (let* ((arg0 (princ-to-string arg))
         (arg1 (safe-key-string arg0)))
    (write-string arg1 stream)))


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
    (format stream "<dtd>/<name> : ~A~%" name))
  (with-truly public-id (model:dtd-public-id dtd)
    (format stream "<dtd>/<public-id> : ~A~%" public-id))
  (with-truly system-id (model:dtd-system-id dtd)
    (format stream "<dtd>/<system-id> : ~A~%" system-id)))


(defun serialize-dtd-items (doc dtd items stream)
  (dolist (item items)
    (when item (serialize-dtd-item doc dtd item stream))))


(defun serialize-dtd-item (doc dtd item stream)
  (declare (ignore doc dtd))
  (typecase item
    (model:attr-decl
     (with-truly element-name (model:attr-decl-element-name item)
       (format stream "<dtd>/<attribute>/<element-name> : ~A~%" element-name))
     (with-truly attribute-name (model:attr-decl-attribute-name item)
       (format stream "<dtd>/<attribute>/<attribute-name> : ~A~%" attribute-name))
     (with-truly type (model:attr-decl-type item)
       (format stream "<dtd>/<attribute>/<type> : ~A~%" type))
     (with-truly default (model:attr-decl-default item)
       (format stream "<dtd>/<attribute>/<default> : ~/regfmt:esc/~%" default)))

    (model:elem-decl
     (with-truly name (model:elem-decl-name item)
       (format stream "<dtd>/<element>/<name> : ~A~%" name))
     (with-truly model (model:elem-decl-model item)
       (format stream "<dtd>/<element>/<model> : ~A~%" model)))

    (model:nota-decl
     (with-truly name (model:nota-decl-name item)
       (format stream "<dtd>/<notation>/<name> : ~A~%" name))
     (with-truly public-id (model:nota-decl-public-id item)
       (format stream "<dtd>/<notation>/<public-id> : ~A~%" public-id))
     (with-truly system-id (model:nota-decl-system-id item)
       (format stream "<dtd>/<notation>/<system-id> : ~A~%" system-id)))

    (model:int-ent-decl
     (with-truly kind (model:int-ent-decl-kind item)
       (format stream "<dtd>/<internal-entity>/<kind> : ~A~%" kind))
     (with-truly name (model:int-ent-decl-name item)
       (format stream "<dtd>/<internal-entity>/<name> : ~A~%" name))
     (with-truly value (model:int-ent-decl-value item)
       (format stream "<dtd>/<internal-entity>/<value> : ~/regfmt:esc/~%" value)))

    (model:ext-ent-decl
     (with-truly kind (model:ext-ent-decl-kind item)
       (format stream "<dtd>/<external-entity>/<kind> : ~A~%" kind))
     (with-truly name (model:ext-ent-decl-name item)
       (format stream "<dtd>/<external-entity>/<name> : ~A~%" name))
     (with-truly public-id (model:ext-ent-decl-public-id item)
       (format stream "<dtd>/<external-entity>/<public-id> : ~A~%" public-id))
     (with-truly system-id (model:ext-ent-decl-system-id item)
       (format stream "<dtd>/<external-entity>/<system-id> : ~A~%" system-id)))

    (model:unp-ent-decl
     (with-truly name (model:unp-ent-decl-name item)
       (format stream "<dtd>/<unparsed-entity>/<name> : ~A~%" name))
     (with-truly public-id (model:unp-ent-decl-public-id item)
       (format stream "<dtd>/<unparsed-entity>/<public-id> : ~A~%" public-id))
     (with-truly system-id (model:unp-ent-decl-system-id item)
       (format stream "<dtd>/<unparsed-entity>/<system-id> : ~A~%" system-id))
     (with-truly nota-name (model:unp-ent-decl-nota-name item)
       (format stream "<dtd>/<unparsed-entity>/<notation-name> : ~A~%" nota-name)))

    (model:unp-int-subs
     (with-truly content (model:unp-int-subs-content item)
       (format stream "<dtd>/<unparsed-internal-subset> : ~/esc/~%" content)))))


(defun serialize-elem-attributes (doc node attributes stream)
  (declare (ignore doc)) ;; TODO
  (dolist (attribute attributes)
    (with-truly local-name (model:attr-local-name attribute)
      (format stream " ~A" local-name))
    (with-truly qname (model:attr-qname attribute)
      (format stream " ~A" qname))
    (with-truly value (model:attr-value attribute)
      (format stream " ~A" value))
    (with-truly namespace-uri (model:attr-namespace-uri attribute)
      (format stream " ~A" namespace-uri))
    (format stream " ~A~%" (model:attr-specified attribute))))


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
