(defpackage #:xmlalt
  (:use #:cl)
  (:export #:main))

(defpackage #:model
  (:use #:cl)
  (:export #:main
   #:uri

   #:doc
   #:doc-xml-decl
   #:doc-dtd
   #:doc-elems

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
   ))

(defpackage #:utils
  (:use #:cl)
  (:export #:call-with-input-stream))

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
