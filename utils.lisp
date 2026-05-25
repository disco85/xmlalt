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


(defun empty-string-p (str)
  "STR is empty string"
  (string= str ""))


(defun truly (x)
  "Like Python's bool."
  (typecase x
    (null nil)
    (number (not (zerop x)))
    (string (string/= x ""))
    (t t)))


(defmacro with-truly (var value &body body)
  "Calls body with bound variable named VAR which value is VALUE only if it is truly (see TRULY)"
  `(let ((,var ,value))
     (when (truly ,var) ,@body)))


; Source - https://stackoverflow.com/a/4367540
; Posted by Svante, modified by community. See post 'Timeline' for change history
; Retrieved 2026-05-18, License - CC BY-SA 4.0:
(defun subs (string part replacement &key (test #'char=))
  "Returns a new STRING in which all the occurences of the PART
is replaced with REPLACEMENT"
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))


(defun non-empty-string-p (s)
  (and (stringp s) (string/= s "")))


(defun empty-string-to-nil (s)
  (check-type s string)
  (if (string= s "") nil s))


(defun try-as-string (s)
  (typecase s
    (null s)
    (string s)
    (keyword (symbol-name s))
    (t (format nil "~A" s))))
