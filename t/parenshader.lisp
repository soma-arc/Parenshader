(in-package :cl-user)
(defpackage parenshader-test
  (:use :cl
        :parenshader
        :prove))
(in-package :parenshader-test)

;; NOTE: To run this test file, execute `(asdf:test-system :parenshader)' in your Lisp.

(plan 6)

(is (analyze 6) '(:expr :value 6 nil))

(is (analyze 'hoge) '(:expr :sym (hoge "hoge") nil))

(is (translate (analyze 3)) "3")

(is (translate (analyze 'hoge)) "hoge")

(is (translate (analyze '(hoge foo bar))) "hoge(foo, bar)")

(is (translate (analyze '(return 2))) "return 2; ")

(finalize)
