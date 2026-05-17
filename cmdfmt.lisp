(in-package :cmdfmt)


(defun serialize (doc stream)
  "Serializes MODEL:DOC object"
  (serialize-dtd doc stream)
  (with-truly root (car (model:doc-elems-stack doc))
              (serialize-nodes doc root stream)))


(defun serialize-dtd (doc stream)
  (with-truly dtd (model:doc-dtd doc)
              (format stream ".DTD~%")
              (serialize-dtd-attrs doc dtd stream)
              (with-truly items (model:dtd-items dtd)
                          (serialize-dtd-items doc dtd items stream))
              (format stream ".DTDE~%")))


(defun serialize-dtd-attrs (doc dtd stream)
  (declare (ignore doc))
  (with-truly name (model:dtd-name dtd)
              (format stream ".DTD NAME ~A~%" name))
  (with-truly public-id (model:dtd-public-id dtd)
              (format stream ".DTD PUB ID ~A~%" public-id))
  (with-truly system-id (model:dtd-system-id dtd)
              (format stream ".DTD SYS ID ~A~%" system-id)))


(defun serialize-dtd-items (doc dtd items stream)
  (dolist (item items)
    (when item (serialize-dtd-item doc dtd item stream))))


(defun serialize-dtd-item (doc dtd item stream)
  (declare (ignore doc dtd))
  (typecase item
    (model:attr-decl
     (with-truly element-name (model:attr-decl-element-name item)
                 (format stream ".DTD ATTR EL NAME ~A~%" element-name))
     (with-truly attribute-name (model:attr-decl-attribute-name item)
                 (format stream ".DTD ATTR ATTR NAME ~A~%" attribute-name))
     (with-truly type (model:attr-decl-type item)
                 (format stream ".DTD ATTR TYPE ~A~%" type))
     (with-truly default (model:attr-decl-default item)
                 (format stream ".DTD ATTR DEF ~A~%" default)))

    (model:elem-decl
     (with-truly name (model:elem-decl-name item)
                 (format stream ".DTD EL NAME ~A~%" name))
     (with-truly model (model:elem-decl-model item)
                 (format stream ".DTD EL MODEL ~A~%" model)))

    (model:nota-decl
     (with-truly name (model:nota-decl-name item)
                 (format stream ".DTD NOTA NAME ~A~%" name))
     (with-truly public-id (model:nota-decl-public-id item)
                 (format stream ".DTD NOTA PUB ID ~A~%" public-id))
     (with-truly system-id (model:nota-decl-system-id item)
                 (format stream ".DTD NOTA SYS ID ~A~%" system-id)))

    (model:int-ent-decl
     (with-truly kind (model:int-ent-decl-kind item)
                 (format stream ".DTD INT ENT KIND ~A~%" kind))
     (with-truly name (model:int-ent-decl-name item)
                 (format stream ".DTD INT ENT NAME ~A~%" name))
     (with-truly value (model:int-ent-decl-value item)
                 (format stream ".DTD INT ENT VALUE ~A~%" value)))

    (model:ext-ent-decl
     (with-truly kind (model:ext-ent-decl-kind item)
                 (format stream ".DTD EXT ENT KIND ~A~%" kind))
     (with-truly name (model:ext-ent-decl-name item)
                 (format stream ".DTD EXT ENT NAME ~A~%" name))
     (with-truly public-id (model:ext-ent-decl-public-id item)
                 (format stream ".DTD EXT PUB ID ~A~%" public-id))
     (with-truly system-id (model:ext-ent-decl-system-id item)
                 (format stream ".DTD EXT SYS ID ~A~%" system-id)))

    (model:unp-ent-decl
     (with-truly name (model:unp-ent-decl-name item)
                 (format stream ".DTD UNP ENT NAME ~A~%" name))
     (with-truly public-id (model:unp-ent-decl-public-id item)
                 (format stream ".DTD UNP ENT PUB ID ~A~%" public-id))
     (with-truly system-id (model:unp-ent-decl-system-id item)
                 (format stream ".DTD UNP ENT SYS ID ~A~%" system-id))
     (with-truly nota-name (model:unp-ent-decl-nota-name item)
                 (format stream ".DTD UNP ENT NOTA NAME ~A~%" nota-name)))

    (model:unp-int-subs
     (with-truly content (model:unp-int-subs-content item)
                 (format stream ".DTD UNP INT SUBS CONT ~A~%" content)))))


(defun serialize-prefix-mappings (doc node prefix-mappings stream)
  (declare (ignore doc node))
  (dolist (pair (model:prefix-mappings-items prefix-mappings))
    (format stream ".EL PREF MAP ~A ~A%"
            (car pair)
            (cdr pair))))


(defun serialize-nodes (doc node stream)
  "Serializes XML node"
  (typecase node
    (model:elem
     (with-truly namespace-uri (model:elem-namespace-uri node)
                 (format stream ".EL NS ~A~%" namespace-uri))
     (with-truly local-name (model:elem-local-name node)
                 (format stream ".EL LOC NAME ~A~%" local-name))
     (with-truly qname (model:elem-qname node)
                 (format stream ".EL Q NAME ~A~%" qname))
     (with-truly prefix-mappings (model:elem-prefix-mappings node)
                 (serialize-prefix-mappings doc node prefix-mappings stream))
     (with-truly children (model:elem-children node)
                 (dolist (child children)
                   (serialize-nodes doc child stream))))

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
                 (format stream ".PI TARG ~A~%" target))
     (with-truly content (model:pinstr-content node)
                 (format stream ".PI~%~A~%.PIE~%" content)))))
