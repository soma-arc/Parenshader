(in-package :cl-user)
(defpackage parenshader
  (:use :cl)
  (:import-from :optima
                :match)
  (:export :analyze
           :translate))
(in-package :parenshader)

(defparameter *pair-analyzers* (make-hash-table))
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

(defun make-analyzer (an-symbol an-literal an-default-pair)
  (labels ((an-pair (expr)
             (let* ((name (car expr))
                    (pair-analyzer (gethash name *pair-analyzers*)))
               (if pair-analyzer
                   (funcall pair-analyzer expr)
                   (funcall an-default-pair expr)))))
    (lambda (expr)
      (cond
        ((symbolp expr) (funcall an-symbol expr))
        ((consp expr)   (an-pair expr))
        (t              (funcall an-literal expr))))))

;;------------------------------------------------------
(defun analyze-symbol (expr)
  (classify :expr :sym (list expr (string expr))))

(defun get-sym-sym (sym-node)
  (first (get-data sym-node)))

(defun get-sym-name (sym-node)
  (second (get-data sym-node)))
;;------------------------------------------------------
(defun analyze-literal (expr)
  (classify :expr :literal expr))

(defun get-literal-value (literal-node)
  (get-data literal-node))
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
                                                #'analyze-literal
                                                #'analyze-pair))

(defun translate (node)
  (funcall (gethash (get-kind node) *translators*)
	   node))

(defun regist-translator (kind f)
  (setf (gethash kind *translators*) f))

(defmacro deftranslator (node kind &body body)
  (let ((f `(lambda (,node) ,@body)))
    `(regist-translator ,kind ,f)))

(deftranslator node :sym
  (get-sym-name node))

(deftranslator node :literal
  (format nil "~a" (get-literal-value node)))

(deftranslator node :call
  (let ((f (translate (get-call-function node)))
	(args (mapcar #'translate (get-call-args node))))
    (format nil "~a(~a)" f (string-join args ", "))))

(defun string-join (list sep)
 (with-output-to-string (s)
   (labels ((fn (list)
              (cond ((null (cdr list)) (princ (car list) s))
                    (t (princ (car list) s)
                       (princ sep s)
                       (fn (cdr list))))))
     (fn list))))

