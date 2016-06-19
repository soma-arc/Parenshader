(in-package :parenshader)
(in-readtable :fare-quasiquote)

(defparameter *macros* (make-hash-table :test 'equal))


(defun expand-macro (name args)
  (apply (gethash (string name) *macros*) args))

(defun macro? (expr)
  (and (symbolp (car expr))
       (gethash (string (car expr)) *macros*)))

(defun pshmacroexpand-1 (expr)
  (let ((expanded? nil))
    (labels ((expander (expr)
	       (cond
		 ((null expr) '())
		 ((not (consp expr)) expr)
		 ((consp (car expr))
		  (cons (expander (car expr))
			(mapcar #'expander (cdr expr))))
		 ((macro? expr)
		  (setf expanded? t)
		  (expand-macro (car expr) (cdr expr)))
		 (t (cons (car expr) (mapcar #'expander (cdr expr)))))))
      (list (expander expr) expanded?))))

(defun pshmacroexpand (expr)
  (let ((expanded expr)
	(expanded? t))
    (loop while expanded? do
	 (let ((e (pshmacroexpand-1 expanded)))
	   (setf expanded (first e))
	   (setf expanded? (second e))))
    expanded))

(defun regist-pshmacro (name m)
  (setf (gethash (string name) *macros*) m))

(defmacro defpshmacro (name args &body body)
  `(regist-pshmacro ',name (lambda ,args ,@body)))

(defun regist-multi-binop (pair)
  (let ((op (cdr pair))
	(primitive (car pair)))
    (regist-pshmacro op
		     (lambda (&rest args)
		       (reduce (lambda (v acc)
				 (if (null acc)
				     v
				     `(,primitive ,v ,acc)))
			       args
			       :initial-value '()
			       :from-end t)))))

(mapcar #'regist-multi-binop +multi-aliases+)
(mapcar #'regist-multi-binop +multi-binops+)

(named-readtables:in-readtable :standard)
