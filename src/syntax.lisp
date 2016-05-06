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

