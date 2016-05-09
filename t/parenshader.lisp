(in-package :cl-user)
(defpackage parenshader-test
  (:use :cl
        :parenshader
        :prove))
(in-package :parenshader-test)

;; NOTE: To run this test file, execute `(asdf:test-system :parenshader)' in your Lisp.

(plan nil)

(subtest "Testing core"
  (is (analyze 6) '(:expr :value 6 nil))

  (is (analyze 'hoge) '(:expr :sym (hoge "hoge") nil))

  (is (translate (analyze 3)) "3")

  (is (translate (analyze 'hoge)) "hoge")

  (is (translate (analyze '(hoge foo bar))) "hoge(foo, bar)"))

(subtest "Testing return"
  (is (translate (analyze '(return 2))) "return 2;"))

(subtest "Testing type"
  (is (translate (analyze '(int 100))) "int 100")
  (is (translate (analyze '(int n 100))) "int n = 100"))

(subtest "Testing defun"
  (is (translate (analyze '(defun hoge int ((int hoge) (int foo))
                            (int f 100))))
      (format nil "int hoge (int hoge, int foo) {~%int f = 100;~%}"))

  (is (translate (analyze '(defun hoge void ((int hoge))
                            (int f 100))))
      (format nil "void hoge (int hoge) {~%int f = 100;~%}")))

(subtest "Testing binop"
  (is (translate (analyze '(>= 1 2)))
      "(1) >= (2)")
  (is (translate (analyze '(<= 1 2)))
      "(1) <= (2)")
  (is (translate (analyze '(= 1 2)))
      "(1) == (2)"))

(subtest "Testing psh"
  (is (psh '((int hoge 1)
             (float foo 2.0)
             (defun add int (a b)
                    (return (binop-plus a b)))
             (add hoge foo)))
      (format nil "int hoge = 1;~%float foo = 2.0;~%int add (a, b) {~%return (a) + (b);~%}~%add(hoge, foo);~%")))

(subtest "Testing setf"
  (is (translate (analyze '(setf hoge (binop-plus 10 10))))
      "hoge = (10) + (10)"))

(subtest "Testing if"
  (is (translate (analyze
                  '(if (= 1 1)
                    (setf hoge 10)
                    (setf hoge 20))))
      (format nil
              "if (~a) {~%~a;~%} else {~%~a;~%}"
              "(1) == (1)"
              "hoge = 10"
              "hoge = 20"))
  (is (translate (analyze '(if (= 1 1)
                            (setf hoge 10))))
      (format nil
              "if (~a) {~%~a;~%}"
              "(1) == (1)"
              "hoge = 10")))

(subtest "Testing when"
  (is (translate (analyze '(when (= 1 1)
                            (setf hoge 100)
                            (setf foo 200))))
      (format nil
              "if (~a) {~%~a~%~a~%}"
              "(1) == (1)"
              "hoge = 100;"
              "foo = 200;")))

(subtest "Testing for"
  (is (translate (analyze '(for ((int i 0) (< i 100) (setf i (binop-plus i 1)))
                            (int hoge 100)
                            (setf hoge 100))))
      (format nil "for (~a; ~a; ~a) {~%~a~%~a~%}"
              "int i = 0" "(i) < (100)" "i = (i) + (1)"
              "int hoge = 100;" "hoge = 100;")))

(finalize)
