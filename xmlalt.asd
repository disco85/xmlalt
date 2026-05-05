(asdf:defsystem #:xmlalt
  :description "Reconstruct directory tree"
  :author "John Doe"
  :license "GPL-3.0-or-later"
  :version "0.0.1"
  :serial t
  :depends-on (:clingon)
  :components ((:file "package")
               (:file "utils")
               (:file "model")
               (:file "xmlalt"))
  :build-operation "program-op"
  :build-pathname "xmlalt"
  :entry-point "xmlalt::main"
  :in-order-to ((test-op (test-op "xmlalt/test"))))

(asdf:defsystem #:xmlalt/test
  :depends-on (:xmlalt :fiveam)
  :components ((:file "xmlalt-test"))
  :perform (test-op (o c) (symbol-call :fiveam :run! :suite1)))
