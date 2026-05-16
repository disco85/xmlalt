(defpackage #:xmlalt
  (:use #:cl)
  (:export #:main))

(defpackage #:model
  (:use #:cl)
  (:export #:main
           #:uri

           #:node
           #:node-dir
           #:node-open-by
           #:node-close-by

           #:doc
           #:doc-xml-decl
           #:doc-dtd
           #:doc-elems-stack

           #:dtd
           #:dtd-items
           #:dtd-name
           #:dtd-public-id
           #:dtd-system-id

           #:attr-decl
           #:attr-decl-element-name
           #:attr-decl-attribute-name
           #:attr-decl-type
           #:attr-decl-default

           #:elem-decl
           #:elem-decl-name
           #:elem-decl-model

           #:nota-decl
           #:nota-decl-name
           #:nota-decl-public-id
           #:nota-decl-system-id

           #:int-ent-decl
           #:int-ent-decl-kind
           #:int-ent-decl-name
           #:int-ent-decl-value

           #:ext-ent-decl
           #:ext-ent-decl-kind
           #:ext-ent-decl-name
           #:ext-ent-decl-public-id
           #:ext-ent-decl-system-id

           #:unp-ent-decl
           #:unp-ent-decl-name
           #:unp-ent-decl-public-id
           #:unp-ent-decl-system-id
           #:unp-ent-decl-nota-name

           #:unp-int-subs
           #:unp-int-subs-content

           #:attr
           #:attr-namespace-uri
           #:attr-local-name
           #:attr-qname
           #:attr-value
           #:attr-value
           #:attr-specified

           #:prefix-mappings
           #:prefix-mappings-items
           #:add-prefix-mappings

           #:elem
           #:elem-namespace-uri
           #:elem-local-name
           #:elem-qname
           #:elem-prefix-mappings
           #:elem-attributes
           #:elem-children

           #:comment
           #:comment-content

           #:text
           #:text-content

           #:cdata
           #:cdata-content
           ))

(defpackage #:utils
  (:use #:cl)
  (:export #:call-with-input-stream
           #:whitespace-char-p))

(defpackage #:blkfmt
  (:use #:cl))

(defpackage #:cmdfmt
  (:use #:cl))

(defpackage #:mrkfmt
  (:use #:cl))

(defpackage #:regfmt
  (:use #:cl))

(defpackage #:xmlfmt
  (:use #:cl))


(defpackage #:xmlalt-tests
  (:use #:cl #:xmlalt)
  )
