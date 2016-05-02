(in-package :cl-user)
(defpackage parenshader-test
  (:use :cl
        :parenshader
        :prove))
(in-package :parenshader-test)

;; NOTE: To run this test file, execute `(asdf:test-system :parenshader)' in your Lisp.

(plan 13)

(is (analyze 6) '(:expr :value 6 nil))

(is (analyze 'hoge) '(:expr :sym (hoge "hoge") nil))

(is (translate (analyze 3)) "3")

(is (translate (analyze 'hoge)) "hoge")

(is (translate (analyze '(hoge foo bar))) "hoge(foo, bar)")

(is (translate (analyze '(return 2))) "return 2; ")

(is (translate (analyze '(int 100))) "int 100")
(is (translate (analyze '(int n 100))) "int n = 100")

(is (translate (analyze '(defun hoge int ((int hoge) (int foo))
                          (int f 100))))
    (format nil "int hoge (int hoge, int foo) {~%int f = 100;~%}~%"))

(is (translate (analyze '(defun hoge void ((int hoge))
                          (int f 100))))
    (format nil "void hoge (int hoge) {~%int f = 100;~%}~%"))

(is (translate (analyze '(>= 1 2)))
    (format nil "(1) >= (2)"))
(is (translate (analyze '(<= 1 2)))
    (format nil "(1) <= (2)"))
(is (translate (analyze '(= 1 2)))
    (format nil "(1) == (2)"))

(finalize)
