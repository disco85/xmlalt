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
  (:export
           #:uri

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

           #:attr-decl
           #:create-attr-decl

           #:nota-decl
           #:create-nota-decl

           #:int-ent-decl
           #:create-int-ent-decl

           #:ext-ent-decl
           #:create-ext-ent-decl

           #:unp-ent-decl
           #:create-unp-ent-decl

           #:unp-int-subs
           #:create-unp-int-subs

           #:dtd
           #:create-dtd
           #:add-dtd-item

           #:doc
           #:create-doc
           #:set-doc-dtd
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
