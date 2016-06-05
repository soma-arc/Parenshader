(in-package :parenshader)
(in-readtable :fare-quasiquote)

(defanalyzer @
  (`(,op ,obj ,@body)
    (declare (ignorable op))
    (classify :expr :chain (list (analyze obj)) (mapcar #'analyze body))))

(defun get-chain-obj (node)
  (first (get-data node)))
(defun get-chain-args (node)
  (get-rest node))

(deftranslator node :chain
  (let* ((obj (translate (get-chain-obj node)))
	 (args (mapcar #'translate (get-chain-args node)))
	 (args (string-join args ".")))
    (format nil "(~a).~a" obj args)))

(in-readtable :standard)
