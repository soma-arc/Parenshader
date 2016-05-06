(in-package :parenshader)

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
   (classify :stat :if (list (analyze test) (analyze true))))
  ((list op test true false)
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

