(in-package :cmdfmt)

(defparameter *dir-delim* "/")


(defun serialize (doc stream)
  "Serializes MODEL:DOC object"
  ;;(serialize-dtd doc stream)
  (with-truly root (model:get-doc-root doc)
    (serialize-nodes
     doc
     root
     stream
     ;; ROOT IS bogus-container, but we check it anywhere (what if the code will be changed?):
     (null (model:get-node-parent root)))))


(defun serialize-dtd (doc dtd stream)
  (when dtd
    (format stream ".DTD~%")
    (serialize-dtd-attrs doc dtd stream)
    (with-truly items (model:get-dtd-items dtd)
      (serialize-dtd-items doc dtd items stream))
    (format stream ".DTDE~%")))


(defun serialize-dtd-attrs (doc dtd stream)
  (declare (ignore doc))
  (with-truly name (model:get-dtd-name dtd)
    (format stream ".DTD.NAME ~A~%" name))
  (with-truly public-id (model:get-dtd-public-id dtd)
    (format stream ".DTD.PUB.ID ~A~%" public-id))
  (with-truly system-id (model:get-dtd-system-id dtd)
    (format stream ".DTD.SYS.ID ~A~%" system-id)))


(defun serialize-dtd-items (doc dtd items stream)
  (dolist (item items)
    (when item (serialize-dtd-item doc dtd item stream))))


(defun serialize-dtd-item (doc dtd item stream)
  (declare (ignore doc dtd))
  (typecase item
    (model:attr-decl
     (with-truly element-name (model:get-attr-decl-elem-name item)
       (format stream ".DTD.ATTR.EL.NAME ~A~%" element-name))
     (with-truly attribute-name (model:get-attr-decl-attr-name item)
       (format stream ".DTD.ATTR.ATTR.NAME ~A~%" attribute-name))
     (with-truly type (model:get-attr-decl-type item)
       (format stream ".DTD.ATTR.TYPE ~A~%" type))
     (with-truly default (model:get-attr-decl-default item)
       (format stream ".DTD.ATTR.DEF ~A~%" default)))

    (model:elem-decl
     (with-truly name (model:get-elem-decl-name item)
       (format stream ".DTD.EL.NAME ~A~%" name))
     (with-truly model (model:get-elem-decl-model item)
       (format stream ".DTD.EL.MODEL ~A~%" model)))

    (model:nota-decl
     (with-truly name (model:get-nota-decl-name item)
       (format stream ".DTD.NOTA.NAME ~A~%" name))
     (with-truly public-id (model:get-nota-decl-public-id item)
       (format stream ".DTD.NOTA.PUB.ID ~A~%" public-id))
     (with-truly system-id (model:get-nota-decl-system-id item)
       (format stream ".DTD.NOTA.SYS.ID ~A~%" system-id)))

    (model:int-ent-decl
     (with-truly kind (model:get-int-ent-decl-kind item)
       (format stream ".DTD.INT.ENT.KIND ~A~%" kind))
     (with-truly name (model:get-int-ent-decl-name item)
       (format stream ".DTD.INT.ENT.NAME ~A~%" name))
     (with-truly value (model:get-int-ent-decl-value item)
       (format stream ".DTD.INT.ENT.VALUE ~A~%" value)))

    (model:ext-ent-decl
     (with-truly kind (model:get-ext-ent-decl-kind item)
       (format stream ".DTD.EXT.ENT.KIND ~A~%" kind))
     (with-truly name (model:get-ext-ent-decl-name item)
       (format stream ".DTD.EXT.ENT.NAME ~A~%" name))
     (with-truly public-id (model:get-ext-ent-decl-public-id item)
       (format stream ".DTD.EXT.PUB.ID ~A~%" public-id))
     (with-truly system-id (model:get-ext-ent-decl-system-id item)
       (format stream ".DTD.EXT.SYS.ID ~A~%" system-id)))

    (model:unp-ent-decl
     (with-truly name (model:get-unp-ent-decl-name item)
       (format stream ".DTD.UNP.ENT.NAME ~A~%" name))
     (with-truly public-id (model:get-unp-ent-decl-public-id item)
       (format stream ".DTD.UNP.ENT.PUB.ID ~A~%" public-id))
     (with-truly system-id (model:get-unp-ent-decl-system-id item)
       (format stream ".DTD.UNP.ENT.SYS.ID ~A~%" system-id))
     (with-truly nota-name (model:get-unp-ent-decl-nota-name item)
       (format stream ".DTD.UNP.ENT.NOTA.NAME ~A~%" nota-name)))

    (model:unp-int-subs
     (with-truly content (model:get-unp-int-subs-content item)
       (format stream ".DTD.UNP.INT.SUBS.CONT ~A~%" content)))))


(defun serialize-elem-attributes (doc attributes stream)
  (declare (ignore doc))
  (dolist (attribute attributes)
    (format stream ".EL.ATTR")
    (with-truly local-name (model:get-attr-local-name attribute)
      (format stream " ~A" local-name))
    (with-truly qname (model:get-attr-qname attribute)
      (format stream " ~A" qname))
    (with-truly value (model:get-attr-value attribute)
      (format stream " ~A" value))
    (with-truly namespace-uri (model:get-attr-namespace-uri attribute)
      (format stream " ~A" (model:write-uri namespace-uri)))
    (format stream " ~A~%" (model:get-attr-specified attribute))))


(defun serialize-prefix-mappings (doc node prefix-mappings stream)
  (declare (ignore doc node))
  (model:over-prefix-mappings
   prefix-mappings
   :do (lambda (pair)
         (format stream ".EL.PREF.MAP ~A ~A~%"
                 (car pair)
                 (cdr pair)))))


(defun serialize-nodes (doc node stream &optional bogus-container)
  "Serializes XML node"
  (typecase node
    (model:dtd
       (serialize-dtd doc node stream))
    (model:elem
       (unless bogus-container
         (format stream ".EL ~A ~A~%"
                 (model:get-elem-children-num node)
                 (model:calc-node-dir node :join-by *dir-delim*))
         (with-truly namespace-uri (model:get-elem-namespace-uri node)
           (format stream ".EL.NS ~A~%" (model:write-uri namespace-uri)))
         (with-truly local-name (model:get-elem-local-name node)
           (format stream ".EL.LOC.NAME ~A~%" local-name))
         (with-truly qname (model:get-elem-qname node)
           (format stream ".EL.QNAME ~A~%" qname))
         (with-truly prefix-mappings (model:get-elem-prefix-mappings node)
           (serialize-prefix-mappings doc node prefix-mappings stream))
         (with-truly attributes (model:get-elem-attributes node)
           (serialize-elem-attributes doc attributes stream)))

       (with-truly children (model:get-elem-children node)
         (dolist (child children)
           (serialize-nodes doc child stream nil)))
       (unless bogus-container
         (format stream ".ELE ~A~%"
                 (model:calc-node-dir node :join-by *dir-delim*))))

    (model:text
       (with-truly content (model:get-text-content node)
         (format stream ".TEXT~%~A~%.TEXTE~%" content)))

    (model:comment
       (with-truly content (model:get-comment-content node)
         (format stream ".COM~%~A~%.COME~%" content)))

    (model:cdata
       (with-truly content (model:get-cdata-content node)
         (format stream ".CD~%~A~%.CDE~%" content)))

    (model:pinstr
       (with-truly target (model:get-pinstr-target node)
         (format stream ".PI.TARG ~A~%" target))
       (with-truly content (model:get-pinstr-data node)
         (format stream ".PI~%~A~%.PIE~%" content)))))


(defun deserialize (in-stream)
  (format t "NOT YET IMPLEMENTED~%"))
