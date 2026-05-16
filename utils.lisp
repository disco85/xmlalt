(in-package :utils)


(defun call-with-input-stream (path fn)
  "Calls a function FN on a PATH which can be '-' which means 'use stdin'"
  (if (and (stringp path)
           (string= path "-"))
      (funcall fn *standard-input*)
      (with-open-file (stream path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (funcall fn stream))))


(defun whitespace-char-p (c)
 (member c '(#\Space #\Tab #\Newline #\Return)))
