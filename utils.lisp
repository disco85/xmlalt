(in-package :xmlalt)


(defun call-with-input-stream (path fn)
  "Calls a function FN on a PATH which can be '-' which means 'use stdin'"
  (if (string= path "-")
      (funcall fn *standard-input*)
      (with-open-file (stream path
                              :direction :input)
        (funcall fn stream))))
