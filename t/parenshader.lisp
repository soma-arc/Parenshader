(in-package :cl-user)
(defpackage parenshader-test
  (:use :cl
        :parenshader
        :prove))
(in-package :parenshader-test)

;; NOTE: To run this test file, execute `(asdf:test-system :parenshader)' in your Lisp.

(plan 6)

(is (analyze 6) '(:expr :literal 6 nil))

(is (analyze 'hoge) '(:expr :sym (hoge "HOGE") nil))

(is (translate (analyze 3)) "3")

(is (translate (analyze 'hoge)) "HOGE")

(is (translate (analyze '(hoge foo bar))) "HOGE(FOO, BAR)")

(is (translate (analyze '(return 2))) "return 2; ")

(finalize)
