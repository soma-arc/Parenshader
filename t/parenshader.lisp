(in-package :cl-user)
(defpackage parenshader-test
  (:use :cl
        :parenshader
        :prove))
(in-package :parenshader-test)

;; NOTE: To run this test file, execute `(asdf:test-system :parenshader)' in your Lisp.

(plan 2)

(is (analyze 6) '(:expr :literal 6 nil))

(is (analyze 'hoge) '(:expr :sym (hoge "HOGE") nil))

(finalize)
