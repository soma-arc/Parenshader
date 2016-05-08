(in-package :parenshader)
(in-readtable :fare-quasiquote)

(defanalyzer setf
  ((list op var val)
   (declare (ignorable op))
   (classify :expr :setf (list (analyze var) (analyze val)))))

(defun get-setf-var (node)
  (first (get-data node)))

(defun get-setf-val (node)
  (second (get-data node)))

(deftranslator node :setf
  (let ((var (translate (get-setf-var node)))
        (val (translate (get-setf-val node))))
    (format nil "~a = ~a" var val)))

(defanalyzer if
  ((list op test true)
   (declare (ignorable op))
   (classify :stat :if (list (analyze test) (analyze true))))
  ((list op test true false)
   (declare (ignorable op))
   (classify :stat :if (list (analyze test) (analyze true) (analyze false)))))

(defun get-if-test (node)
  (first (get-data node)))

(defun get-if-true (node)
  (second (get-data node)))

(defun get-if-false (node)
  (if (= (length (get-data node)) 3)
      (third (get-data node))))

(deftranslator node :if
  (let* ((test (translate (get-if-test node)))
         (true (translate (get-if-true node)))
         (false (get-if-false node))
         (false (if false (translate false))))
    (if false
        (format nil "if (~a) {~%~a;~%} else {~%~a;~%}" test true false)
        (format nil "if (~a) {~%~a;~%}" test true))))

(defanalyzer when
  (`(,op ,test ,@body)
    (declare (ignorable op))
    (classify :stat :when (list (analyze test)) (mapcar #'analyze body))))

(defun get-when-test (node)
  (car (get-data node)))
(defun get-when-body (node)
  (get-rest node))

(deftranslator node :when
  (let ((test (translate (get-when-test node)))
        (body (translate-body (get-when-body node))))
    (format nil "if (~a) {~%~a}" test body)))

(in-readtable :standard)
