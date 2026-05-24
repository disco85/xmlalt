(defpackage #:utils
  (:use #:cl)
  (:export #:call-with-input-stream
           #:whitespace-char-p
           #:with-truly
           #:subs))

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

           #:attr
           #:create-attr

           #:text
           #:create-text

           #:pinstr
           #:create-pinstr

           #:cdata
           #:create-cdata

           #:comment
           #:create-comment

           #:prefix-mappings
           #:create-prefix-mappings
           #:add-prefix-mappings
           #:over-prefix-mappings

           #:elem
           #:create-elem
           #:elem-children-num
           #:over-elem-children
           #:add-child-node-to-current-elem
           #:enter-elem
           #:exit-from-elem

           #:doctype
           #:create-doctype

           #:xml-decl
           #:create-xml-decl

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
           ))

(defpackage #:blkfmt
  (:use #:cl #:utils))

(defpackage #:cmdfmt
  (:use #:cl #:utils)
  (:export #:serialize))

(defpackage #:mrkfmt
  (:use #:cl #:utils))

(defpackage #:regfmt
  (:use #:cl #:utils)
  (:export #:serialize
           #:esc))

(defpackage #:xmlfmt
  (:use #:cl #:utils))


(defpackage #:xmlalt-tests
  (:use #:cl #:xmlalt #:utils))
