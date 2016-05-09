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

(defanalyzer cond
  (`(,op ,@clauses)
    (declare (ignorable op))
    (let* ((conditions '())
           (else-body nil))
      (loop for clause in clauses do
           (optima:match clause
             (`(t ,@body)
               (setf else-body (mapcar #'analyze body)))
             (`(,test ,@body)
               (push (list (analyze test) (mapcar #'analyze body))
                     conditions))))
      (classify :stat :cond (list (nreverse conditions) else-body)))))

(defun get-cond-conditions (node)
  (first (get-data node)))
(defun get-cond-else-body (node)
  (second (get-data node)))

(deftranslator node :cond
  (let* ((conditions (mapcar (lambda (condition)
                               (list (translate (first condition))
                                     (translate-body (second condition))))
                             (get-cond-conditions node)))
         (else-body (get-cond-else-body node))
         (else-body (if else-body (translate-body else-body) nil))
         (result '()))
    (cond ((and (= 0 (length conditions)) else-body)
           else-body)
          (t
           (push (format nil "if (~a) {~%~a}"
                         (caar conditions) (second (first conditions)))
                 result)
           (loop for condition in (cdr conditions) do
                (push (format nil " else if (~a) {~%~a}"
                              (first condition) (second condition))
                      result))
           (if else-body (push (format nil " else {~%~a}" else-body)
                               result))
           (string-join (nreverse result))))))

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

(defanalyzer for
  (`(,op ,(list init test fin) ,@body)
    (declare (ignorable op))
    (classify :stat :for
              (list (analyze init) (analyze test) (analyze fin))
              (mapcar #'analyze body))))

(defun get-for-init (node)
  (first (get-data node)))
(defun get-for-test (node)
  (second (get-data node)))
(defun get-for-fin (node)
  (third (get-data node)))
(defun get-for-body (node)
  (get-rest node))

(deftranslator node :for
  (let* ((init (translate (get-for-init node)))
         (test (translate (get-for-test node)))
         (fin (translate (get-for-fin node)))
         (body (translate-body (get-for-body node))))
    (format nil "for (~a; ~a; ~a) {~%~a}"
            init test fin body)))

(in-readtable :standard)
