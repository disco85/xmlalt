(in-package :xmlalt)


(defun cli-main-cmd-handler (cmd)
  (declare (ignorable cmd))
  (let* ((dry-run (clingon:getopt cmd :dry))
         (verbose-run (clingon:getopt cmd :verbose))
         (prepend-with (clingon:getopt cmd :prepend))
         (input-lines (read-stdin-lines))
         (parsed-lines (acl2::parse-tree-output input-lines))
         (classified-lines (acl2::classify-tree-output parsed-lines))
         (prepended-classified-lines
           (if prepend-with
               (acl2::prepend-classified-tree-output prepend-with classified-lines)
               classified-lines)))
    ;; (format t "~{~A~%~}~%" prepended-classified-lines)
    (ensure-classified-tree-output prepended-classified-lines :immediately dry-run verbose-run)))



(defun cli-main-cmd-opts ()
  (list
   (clingon:make-option
    :string
    :description "Prepend paths with common directory-prefix"
    :short-name #\p
    :long-name "prepend"
    :key :prepend)
   (clingon:make-option
    :flag
    :description "Dry run"
    :short-name #\d
    :long-name "dry"
    :key :dry)
   (clingon:make-option
    :flag
    :description "Verbose run"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)))


(defun cli-main-cmd ()
  (clingon:make-command
   :name "xmlalt"
   :description "XML serial alternative formats converter"
   :usage "-i,--in FILE|- -o,--out FILE|- -f,--format CMD,REG"
   :examples '(("Do X" . "xmlalt -x")
               ("Do Y" . "xmlalt -y"))
   :version "0.1.0"
   :authors '("John Doe <john.doe@example.org>")
   :license "GPL-3.0-or-later"
   :options (cli-main-cmd-opts)
   :handler #'cli-main-cmd-handler))


(defun main ()
  (let* ((argv (uiop:command-line-arguments))
         (argv1 (loop for arg in argv
                      if (string= arg "-h")
                        collect "--help"
                      else
                        collect arg)))
    (clingon:run (cli-main-cmd) argv1)))

;; (defun main ()
;;   (let* ((parsed (xmlfmt::parse-xml #P"./t1.xml"))
;;          (res (car parsed)))
;;     (if (eq res :ok)
;;         (progn ;(inspect parsed)
;;                (cmdfmt:serialize (cdr parsed) t)
;;                (format t "~%~%~%")
;;                (regfmt:serialize (cdr parsed) t))
;;         (format t "ERROR: ~A~%" (cdr parsed))
;;         )))
