(in-package :parenshader)
(in-readtable :fare-quasiquote)

;;return
(defanalyzer return ((list op expr)
                     (declare (ignorable op))
                     (classify :stat :return '() (analyze expr))))

(defun get-return-expr (node)
  (get-rest node))

(deftranslator node :return
  (format nil "return ~a; " (translate (get-return-expr node))))

;;defun
(defanalyzer defun (`(,op ,name ,type ,args ,@body)
                    (declare (ignorable op))
                     (classify :stat :defun
                               (list type (mapcar #'analyze args) name)
                               (mapcar #'analyze body))))

(defun get-fun-type (node)
  (first (get-data node)))

(defun get-fun-args (node)
  (second (get-data node)))

(defun get-fun-name (node)
  (third (get-data node)))

(defun get-fun-body (node)
  (get-rest node))

(deftranslator node :defun
  (let* ((type (string-downcase (get-fun-type node)))
         (name (string-downcase (get-fun-name node)))
         (args (translate-args (get-fun-args node)))
         (body (translate-body (get-fun-body node))))
    (format nil "~a ~a (~a) {~%~a}~%" type name args body)))

(named-readtables:in-readtable :standard)
