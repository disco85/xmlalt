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


;; (defun nzstr (str)
;;   "STR is not-empty string"
;;   (string/= str ""))


(defun truly (x)
  "Like Python's bool."
  (typecase x
    (null nil)
    (number (not (zerop x)))
    (string (string/= x ""))
    (t t)))


(defmacro with-truly (var value &body body)
  `(let ((,var ,value))
     (when (truly ,var) ,@body)))
