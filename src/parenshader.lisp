(in-package :cl-user)
(defpackage parenshader
  (:use :cl)
  (:import-from :optima
                :match)
  (:export :analyze))
(in-package :parenshader)

(defparameter *pair-analyzers* (make-hash-table))

(defun classify (tag kind data &optional (rest '()))
  (list tag kind data rest))

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

(defun analyze-symbol (expr)
  (classify :expr :sym (list expr (string expr))))

(defun analyze-literal (expr)
  (classify :expr :literal expr))

(defun analyze-pair (expr)
  (classify :expr
            :call
            (list (analyze (car expr)))
            (mapcar #'analyze (cdr expr))))

(setf (symbol-function 'analyze) (make-analyzer #'analyze-symbol
                                                #'analyze-literal
                                                #'analyze-pair))
