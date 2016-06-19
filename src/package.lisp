(in-package :cl-user)

(defpackage parenshader
  (:use :cl
	:optima.extra
	:fare-quasiquote
	:named-readtables
	:cl-ppcre)
  (:export #:analyze
           #:translate
           #:psh
	   #:pshmacroexpand
	   #:pshmacroexpand-1))
