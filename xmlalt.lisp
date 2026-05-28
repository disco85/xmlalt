(in-package :xmlalt)

;; Supported formats:
(defparameter +formats+ '(:cmdfmt :regfmt :xmlfmt))


(defun format-serialize-func (format)
  (ecase format
    (:cmdfmt #'cmdfmt:serialize)
    (:regfmt #'regfmt:serialize)
    (:xmlfmt #'xmlfmt:serialize)))


(defun format-deserialize-func (format)
  (ecase format
    (:cmdfmt #'cmdfmt:deserialize)
    (:regfmt #'regfmt:deserialize)
    (:xmlfmt #'xmlfmt:deserialize)))


(defun cli-kw-getopt (cmd key)
  "Allows to get a choice/enum option as a symbol in :keyword package ready for eq/getf/..."
  (intern (string-upcase (clingon:getopt cmd key)) :keyword))


(defun cli-main-cmd-handler (cmd)
  (declare (ignorable cmd))
  (let* ((in-path (clingon:getopt cmd :in))
         (out-path (clingon:getopt cmd :out))
         (from-format (cli-kw-getopt cmd :from))
         (to-format (cli-kw-getopt cmd :to))
         (parsed nil)
         (dir-delim (clingon:getopt cmd :dir-delim)))
    (when dir-delim
      (setf cmdfmt:*dir-delim* dir-delim))
    ;; (format t "RUN: ~A ~A ~A ~A~%" in out from to)
    (with-input-stream (in-stream in-path)
      (setf parsed (funcall (format-deserialize-func from-format)
                            in-stream)))
    (ecase (car parsed)
      (:ok (let ((doc (cdr parsed)))
             (with-output-stream (out-stream out-path)
               (funcall (format-serialize-func to-format)
                        doc
                        out-stream))))
      (t (format t "ERROR: ~A~%" (cdr parsed))))

    t))


;; TODO add options for delimiters
(defun cli-main-cmd-opts ()
  (list
   (clingon:make-option
    :string
    :description "Input file [missing option for stdin]"
    :short-name #\i
    :long-name "in"
    :key :in)
   (clingon:make-option
    :string
    :description "Output file [missing option for stdout]"
    :short-name #\o
    :long-name "out"
    :key :out)
   (clingon:make-option
    :choice
    :required t
    :description "From format"
    :short-name #\f
    :long-name "from"
    :key :from
    ;; :initial-value :xmlfmt
    :items +formats+)
   (clingon:make-option
    :choice
    :required t
    :description "To format"
    :short-name #\t
    :long-name "to"
    :key :to
    ;; :initial-value :cmdfmt
    :items +formats+)
   (clingon:make-option
    :string
    :description "Directory delimiter string [default is /]"
    :short-name #\d
    :long-name "dir-delim"
    :key :dir-delim)))


(defun cli-main-cmd ()
  (clingon:make-command
   :name "xmlalt"
   :description "XML serial alternative formats converter"
   :usage "[-i|--in FILE] [-o|--out FILE] -f|--from CMDFMT|REGFMT|XMLFMT -t|--to CMDFMT|REGFMT|XMLFMT
         [-d|--dir-delim STRING]"
   :examples
   '(("Convert XML to some serial format from stdin to stdout" . "cat a.xml|xmlalt -f XMLFMT -t CMDFMT")
     ("Convert XML to some serial format from stdin to stdout with a custom directory delimiter"
      . "cat a.xml|xmlalt -f XMLFMT -t CMDFMT -d::")
     ("Convert a serial format from a file to XML to a file" . "xmlalt -i a.reg -f REGFMT -t XMLFMT -o a.xml"))
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
