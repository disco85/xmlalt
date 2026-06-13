(in-package :utils)


;; (defun call-with-input-stream (path fn)
;;   "Calls a function FN on a PATH which can be '-' or NIL which means 'use stdin'"
;;   (if (or (null path)
;;           (and (stringp path)
;;                (string= path "-")))
;;       (funcall fn *standard-input*)
;;       (with-open-file (stream path
;;                               :direction :input
;;                               :element-type '(unsigned-byte 8))
;;         (funcall fn stream))))

(defmacro with-input-stream ((var path &key (mode :binary))
                             &body body)
  "VAR is the name of the STREAM variable to be used inside BODY"
  (let ((p (gensym "PATH")))
    `(let ((,p ,path))
       (if (or (null ,p)
               (and (stringp ,p)
                    (string= ,p "-")))
           (let ((,var *standard-input*))
             ,@body)
           (with-open-file
               (,var ,p
                     :direction :input
                     :element-type
                     ,(ecase mode
                        (:binary ''(unsigned-byte 8))
                        (:text ''character))
                     :external-format
                     ,(ecase mode
                        (:binary :default)
                        (:text :utf-8)))
             ,@body)))))


(defmacro with-output-stream ((stream path &key (mode :binary))
                              &body body)
  (let ((p (gensym "PATH")))
    `(let ((,p ,path))
       (if (or (null ,p)
               (and (stringp ,p)
                    (string= ,p "-")))
           (let ((,stream *standard-output*))
             ,@body)
           (with-open-file
               (,stream ,p
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :element-type
                        ,(ecase mode
                           (:binary ''(unsigned-byte 8))
                           (:text ''character)))
             ,@body)))))


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
  "Returns a new STRING in which all the occurences of the PART substring
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
