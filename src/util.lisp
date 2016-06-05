(in-package :parenshader)

(defun string-join (list &optional (sep ""))
 (with-output-to-string (s)
   (labels ((fn (list)
              (cond ((null (cdr list)) (princ (car list) s))
                    (t (princ (car list) s)
                       (princ sep s)
                       (fn (cdr list))))))
     (fn list))))

(defun string-camelcase (str)
  (let* ((splitted (split "-" str))
	 (head (car splitted))
	 (tail (mapcar #'string-capitalize (cdr splitted))))
    (apply #'concatenate 'string head tail)))

(defun string->glsl-const-sym (str)
  (multiple-value-bind
	(matchp matched)
      (scan-to-strings "\\+(.*)\\+" str)
    (if matchp
	(regex-replace-all "-" (string-upcase (aref matched 0)) "_")
	str)))

(defparameter +replacers+
  (let* ((replacers '(("!" "_BANG_")
		      ("\\?" "_P_")
		      ("\\*" "_STAR_")
		      ("&" "_AMP_")
		      ("@" "_AT_")
		      ("\\/" "_SLASH_")
		      ("<" "_LT_")
		      (">" "_GT_")
		      ("\\^" "_CARET_")
		      ("%" "_PREC_")
		      ("\\+" "_PLUS_"))))
    (nreverse
     (apply #'list
	    #'string->glsl-const-sym
	    #'string-camelcase
	    (apply #'mapcar
		   (lambda (regex replacement)
		     (lambda (str)
		       (regex-replace-all regex str replacement)))
		   (list (mapcar #'first replacers)
			 (mapcar #'second replacers)))))))

(defun string->glsl-sym (s)
  (reduce (lambda (f str)
	    (funcall f str)) +replacers+
	    :initial-value (string-downcase s)
	    :from-end t))

(defun symbol->glsl-sym (sym)
  (string->glsl-sym (string sym)))
