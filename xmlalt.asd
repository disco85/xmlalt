;; TODO fix <..> escaping
;; TODO CLI
;; TODO load of cmdfmt, regfmt

(asdf:defsystem #:xmlalt
  :description "Reconstruct directory tree"
  :author "John Doe"
  :license "GPL-3.0-or-later"
  :version "0.0.1"
  :serial t
  :depends-on (:clingon :cxml :flexi-streams)
  :components ((:file "package")
               (:file "utils")
               (:file "model")
               (:file "cmdfmt")
               (:file "regfmt")
               (:file "xmlfmt")
               (:file "xmlalt"))
  :build-operation "program-op"
  :build-pathname "xmlalt"
  :entry-point "xmlalt::main"
  :in-order-to ((test-op (test-op "xmlalt/test"))))

(asdf:defsystem #:xmlalt/test
  :depends-on (:xmlalt :fiveam)
  :components ((:file "xmlalt-test"))
  :perform (test-op (o c) (symbol-call :fiveam :run! :suite1)))
