(in-package :cl-user)

(defpackage parenshader
  (:use :cl :optima.extra :fare-quasiquote :named-readtables :split-sequence)
  (:export #:analyze
           #:translate))
