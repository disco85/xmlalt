(defpackage #:utils
  (:use #:cl)
  (:export #:call-with-input-stream
           #:whitespace-char-p
           #:with-truly
           #:subs
           #:non-empty-string-p
           #:empty-string-to-nil
           #:try-as-string
           #:with-input-stream
           #:with-output-stream))

(defpackage #:xmlalt
  (:use #:cl #:utils)
  (:export #:main))

(defpackage #:model
  (:use #:cl #:utils)
  (:export #:uri
           #:write-uri

           #:node
           #:create-node
           #:set-node-idx
           #:calc-node-dir
           #:get-node-idx
           #:get-node-open-by
           #:get-node-close-by
           #:get-node-parent

           #:attr
           #:create-attr
           #:get-attr-namespace-uri
           #:get-attr-local-name
           #:get-attr-qname
           #:get-attr-value
           #:get-attr-specified

           #:text
           #:create-text
           #:get-text-content

           #:pinstr
           #:create-pinstr
           #:get-pinstr-target
           #:get-pinstr-data

           #:cdata
           #:create-cdata
           #:get-cdata-content

           #:comment
           #:create-comment
           #:get-comment-content

           #:prefix-mappings
           #:create-prefix-mappings
           #:add-prefix-mappings
           #:over-prefix-mappings

           #:elem
           #:create-elem
           #:get-elem-children-num
           #:over-elem-children
           #:add-child-node-to-current-elem
           #:enter-elem
           #:exit-from-elem
           #:get-elem-namespace-uri
           #:get-elem-local-name
           #:get-elem-qname
           #:get-elem-prefix-mappings
           #:get-elem-uniq-prefix-mappings
           #:get-elem-attributes
           #:get-elem-children

           #:doctype
           #:create-doctype

           #:xml-decl
           #:create-xml-decl
           #:get-xml-decl-content

           #:dtd-item

           #:elem-decl
           #:create-elem-decl
           #:get-elem-decl-name
           #:get-elem-decl-model

           #:attr-decl
           #:create-attr-decl
           #:get-attr-decl-elem-name
           #:get-attr-decl-attr-name
           #:get-attr-decl-type
           #:get-attr-decl-default

           #:nota-decl
           #:create-nota-decl
           #:get-nota-decl-name
           #:get-nota-decl-public-id
           #:get-nota-decl-system-id

           #:int-ent-decl
           #:create-int-ent-decl
           #:get-int-ent-decl-kind
           #:get-int-ent-decl-name
           #:get-int-ent-decl-value

           #:ext-ent-decl
           #:create-ext-ent-decl
           #:get-ext-ent-decl-kind
           #:get-ext-ent-decl-name
           #:get-ext-ent-decl-public-id
           #:get-ext-ent-decl-system-id

           #:unp-ent-decl
           #:create-unp-ent-decl
           #:get-unp-ent-decl-name
           #:get-unp-ent-decl-public-id
           #:get-unp-ent-decl-system-id
           #:get-unp-ent-decl-nota-name

           #:unp-int-subs
           #:create-unp-int-subs
           #:get-unp-int-subs-content

           #:dtd
           #:create-dtd
           #:add-dtd-item
           #:get-dtd-items
           #:get-dtd-name
           #:get-dtd-public-id
           #:get-dtd-system-id

           #:doc
           #:create-doc
           #:set-doc-dtd
           #:get-doc-root
           #:get-doc-dtd
           #:get-doc-xml-decl
           #:set-doc-xml-decl
           ))

(defpackage #:cmdfmt
  (:use #:cl #:utils)
  (:export #:serialize
           #:deserialize
           #:*dir-delim*))

(defpackage #:regfmt
  (:use #:cl #:utils)
  (:export #:serialize
           #:deserialize
           #:esc))

(defpackage #:xmlfmt
  (:use #:cl #:utils)
  (:export #:serialize
           #:deserialize))


(defpackage #:xmlalt-tests
  (:use #:cl #:xmlalt #:utils))
