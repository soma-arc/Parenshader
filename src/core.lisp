(in-package :parenshader)
(in-readtable :fare-quasiquote)

(defparameter *pair-analyzers* (make-hash-table :test 'equal))
(defparameter *translators*  (make-hash-table))

(defun classify (tag kind data &optional (rest '()))
  (list tag kind data rest))
(defun get-tag (node)
  (first node))
(defun get-kind (node)
  (second node))
(defun get-data (node)
  (third node))
(defun get-rest (node)
  (fourth node))

;;an-symbol        -- analyzer for symbol
;;an-value       -- analyzer for value
;;ann-default-pair -- analyzer for default pair
(defun make-analyzer (an-symbol an-value an-default-pair)
  (labels ((an-pair (expr)
             (let* ((name (car expr))
                    (pair-analyzer (gethash (string name) *pair-analyzers*)))
               (if pair-analyzer
                   (funcall pair-analyzer expr)
                   (funcall an-default-pair expr)))))
    (lambda (expr)
      (cond
        ((symbolp expr) (funcall an-symbol expr))
        ((consp expr)   (an-pair expr))
        (t              (funcall an-value expr))))))

;;------------------------------------------------------
(defun analyze-symbol (expr)
  (classify :expr :sym (list expr (string-downcase (string expr)))))

(defun get-sym-sym (sym-node)
  (first (get-data sym-node)))

(defun get-sym-name (sym-node)
  (second (get-data sym-node)))
;;------------------------------------------------------
(defun analyze-value (expr)
  (classify :expr :value expr))

(defun get-value-value (value-node)
  (get-data value-node))
;;------------------------------------------------------

(defun analyze-pair (expr)
  (classify :expr
            :call
            (list (analyze (car expr)))
            (mapcar #'analyze (cdr expr))))

(defun get-call-function (call-node)
  (first (get-data call-node)))
(defun get-call-args (call-node)
  (get-rest call-node))

(setf (symbol-function 'analyze) (make-analyzer #'analyze-symbol
                                                #'analyze-value
                                                #'analyze-pair))

(defun translate (node)
  (funcall (gethash (get-kind node) *translators*)
	   node))

(defun translate-args (args)
  (string-join (mapcar #'translate args) ", "))

(defun translate-body-node (node)
  (let ((str (translate node)))
    (format nil "~A~A~%" str (if (eq (get-tag node) :expr) ";" ""))))

(defun translate-body (body)
  (string-join (mapcar #'translate-body-node body)))

(defun regist-translator (kind f)
  (setf (gethash kind *translators*) f))

(defmacro deftranslator (node kind &body body)
  (let ((f `(lambda (,node) ,@body)))
    `(regist-translator ,kind ,f)))

(deftranslator node :sym
  (get-sym-name node))

(deftranslator node :value
  (format nil "~a" (get-value-value node)))

(deftranslator node :call
  (let ((f (translate (get-call-function node)))
	(args (mapcar #'translate (get-call-args node))))
    (format nil "~a(~a)" f (string-join args ", "))))

(defun string-join (list &optional (sep ""))
 (with-output-to-string (s)
   (labels ((fn (list)
              (cond ((null (cdr list)) (princ (car list) s))
                    (t (princ (car list) s)
                       (princ sep s)
                       (fn (cdr list))))))
     (fn list))))


(defun regist-pair-analyzer (sym f)
  (setf (gethash (string sym) *pair-analyzers*) f))

(defmacro defanalyzer (sym &body clauses)
  `(regist-pair-analyzer ',sym
                         (optima.extra:lambda-match ,@clauses)))

(defun psh (expr)
  (string-join
   (mapcar (lambda (body)
             (translate-body-node
              (analyze body)))
           expr)))

(named-readtables:in-readtable :standard)
