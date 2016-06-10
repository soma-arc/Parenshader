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

  (is (translate (analyze '(int hoge-foo-bar)))
      "int hogeFooBar")

  (is (translate (analyze '(int +hoge-foo-bar+)))
      "int HOGE_FOO_BAR")

  (is (translate (analyze '(int +hoge+bar+)))
      "int HOGE_PLUS_BAR")

  (is (translate (analyze '(int foo!)))
      "int foo_BANG_")
  
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
      (format nil "void hoge (int hoge) {~%int f = 100;~%}"))
  (is (translate (analyze '(defun set! void ((int hoge-foo))
                            (int f hoge-foo))))
      (format nil "void set_BANG_ (int hogeFoo) {~%int f = hogeFoo;~%}")))

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

(subtest "Testing cond"
  (is (translate (analyze '(cond
                            ((< i 10)
                             (setf hoge 100)
                             (setf foo (binop-plus 100 bar)))
                            ((< i 20)
                             (setf hoge 200)
                             (setf foo (binop-plus 200 bar)))
                            (t
                             (setf hoge 0)
                             (return hoge)))))
      (format nil "if (~a) {~%~a~%} else if (~a) {~%~a~%} else {~%~a~%}"
              "(i) < (10)" (format nil "hoge = 100;~%foo = (100) + (bar);")
              "(i) < (20)" (format nil "hoge = 200;~%foo = (200) + (bar);")
              (format nil "hoge = 0;~%return hoge;")))
  (is (translate (analyze '(cond
                            ((< i 10)
                             (setf hoge 100)
                             (setf foo (binop-plus 100 bar)))
                            ((< i 20)
                             (setf hoge 200)
                             (setf foo (binop-plus 200 bar))))))
      (format nil "if (~a) {~%~a~%} else if (~a) {~%~a~%}"
              "(i) < (10)" (format nil "hoge = 100;~%foo = (100) + (bar);")
              "(i) < (20)" (format nil "hoge = 200;~%foo = (200) + (bar);")))
  (is (translate (analyze '(cond
                            ((< i 10)
                             (setf hoge 100)
                             (setf foo (binop-plus 100 bar))))))
      (format nil "if (~a) {~%~a~%}"
              "(i) < (10)" (format nil "hoge = 100;~%foo = (100) + (bar);")))
  (is (translate (analyze '(cond
                            (t
                             (setf hoge 0)
                             (return hoge)))))
      (format nil "~a~%"
              (format nil "hoge = 0;~%return hoge;"))))

(subtest "Testing chain"
  (is (translate (analyze '(@ vec x)))
      (format nil "(vec).x"))
  (is (translate (analyze '(@ vec xxy)))
      (format nil "(vec).xxy")))

(subtest "Testing var"
  (is (psh '((uniform vec3 hoge)
	     (varying float foo)
	     (attribute int i)))
      (format nil "~a~%~a~%~a~%"
	      "uniform vec3 hoge;"
	      "varying float foo;"
	      "attribute int i;")))

(subtest "Testing type-conv"
  (is (translate (analyze '(@vec2 1 2)))
      "vec2(1, 2)")
  (is (translate (analyze '(@vec3 1 2 3)))
      "vec3(1, 2, 3)")
  (is (translate (analyze '(@vec4 hoge 2 3)))
      "vec4(hoge, 2, 3)"))

(finalize)
