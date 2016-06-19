(in-package :parenshader)
(in-readtable :fare-quasiquote)

(defun an-var ()
  (optima.extra:lambda-match
    ((list op type var)
     (classify :stat :var (list (analyze op) (analyze type) (analyze var))))))

(mapcar (lambda (var)
	  (regist-pair-analyzer var (an-var)))
	'(uniform varying attribute in out))

(defun get-var-name (node)
  (first (get-data node)))
(defun get-var-type (node)
  (second (get-data node)))
(defun get-var-var (node)
  (third (get-data node)))

(deftranslator node :var
  (format nil "~a ~a ~a;"
	  (translate (get-var-name node))
	  (translate (get-var-type node))
	  (translate (get-var-var node))))

(defanalyzer const ((list op expr)
		    (declare (ignorable op))
		    (classify :stat :const '() (analyze expr))))

(defun get-const-expr (node)
  (get-rest node))

(deftranslator node :const
  (format nil "const ~a;" (translate (get-const-expr node))))

(in-readtable :standard)
