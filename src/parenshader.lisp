(in-package :cl-user)
(defpackage parenshader
  (:use :cl :optima.extra :fare-quasiquote :named-readtables :split-sequence)
  (:export #:analyze
           #:translate
           :+types+
           :*pair-analyzers*))
(in-package :parenshader)
(in-readtable :fare-quasiquote)

(defparameter *pair-analyzers* (make-hash-table :test 'equal))
(defparameter *translators*  (make-hash-table))

(defun classify (tag kind data &optional (rest '()))
  (list tag kind data rest))
(defun get-tag (node)
  (first node))
(defun get-kind (node)
  (second node))
(defun get-data (node)
  (third node))
(defun get-rest (node)
  (fourth node))


;;an-symbol        -- analyzer for symbol
;;an-value       -- analyzer for value
;;ann-default-pair -- analyzer for default pair
(defun make-analyzer (an-symbol an-value an-default-pair)
  (labels ((an-pair (expr)
             (let* ((name (car expr))
                    (pair-analyzer (gethash (string name) *pair-analyzers*)))
               (if pair-analyzer
                   (funcall pair-analyzer expr)
                   (funcall an-default-pair expr)))))
    (lambda (expr)
      (cond
        ((symbolp expr) (funcall an-symbol expr))
        ((consp expr)   (an-pair expr))
        (t              (funcall an-value expr))))))

;;------------------------------------------------------
(defun analyze-symbol (expr)
  (classify :expr :sym (list expr (string-downcase (string expr)))))

(defun get-sym-sym (sym-node)
  (first (get-data sym-node)))

(defun get-sym-name (sym-node)
  (second (get-data sym-node)))
;;------------------------------------------------------
(defun analyze-value (expr)
  (classify :expr :value expr))

(defun get-value-value (value-node)
  (get-data value-node))
;;------------------------------------------------------

(defun analyze-pair (expr)
  (classify :expr
            :call
            (list (analyze (car expr)))
            (mapcar #'analyze (cdr expr))))

(defun get-call-function (call-node)
  (first (get-data call-node)))
(defun get-call-args (call-node)
  (get-rest call-node))

(setf (symbol-function 'analyze) (make-analyzer #'analyze-symbol
                                                #'analyze-value
                                                #'analyze-pair))

(defun translate (node)
  (funcall (gethash (get-kind node) *translators*)
	   node))

(defun regist-translator (kind f)
  (setf (gethash kind *translators*) f))

(defmacro deftranslator (node kind &body body)
  (let ((f `(lambda (,node) ,@body)))
    `(regist-translator ,kind ,f)))

(deftranslator node :sym
  (get-sym-name node))

(deftranslator node :value
  (format nil "~a" (get-value-value node)))

(deftranslator node :call
  (let ((f (translate (get-call-function node)))
	(args (mapcar #'translate (get-call-args node))))
    (format nil "~a(~a)" f (string-join args ", "))))

(defun string-join (list sep)
 (with-output-to-string (s)
   (labels ((fn (list)
              (cond ((null (cdr list)) (princ (car list) s))
                    (t (princ (car list) s)
                       (princ sep s)
                       (fn (cdr list))))))
     (fn list))))


(defun regist-pair-analyzer (sym f)
  (setf (gethash (string sym) *pair-analyzers*) f))

(defmacro defanalyzer (sym &body clauses)
  `(regist-pair-analyzer ',sym
                         (optima.extra:lambda-match ,@clauses)))

;;return
(defanalyzer return ((list op expr)
                     (declare (ignorable op))
                     (classify :stat :return '() (analyze expr))))

(defun get-return-expr (node)
  (get-rest node))

(deftranslator node :return
  (format nil "return ~a; " (translate (get-return-expr node))))

;;type
(defvar +types+
  '(int uint bool float double

    vec2 vec3 vec4 dvec2 dvec3 dvec4 bvec2 bvec3 bvec4
    ivec2 ivec3 ivec4 uvec2 uvec3 uvec4

    mat2 mat3 mat4 mat2x2 mat2x3 mat2x4
    mat3x2 mat3x3 mat3x4 mat4x2 mat4x3 mat4x4

    dmat2 dmat3 dmat4 dmat2x2 dmat2x3 dmat2x4
    dmat3x2 dmat3x3 dmat3x4 dmat4x2 dmat4x3 dmat4x4

    sampler2D sampler3D sampler4D
    samplerCube sampler2DRect sampler1DArray sampler2DArray
    samplerBuffer sampler2DMS sampler2DMSArray samplerCubeArray

    image2D image3D image4D
    imageCube image2DRect image1DArray image2DArray
    imageBuffer image2DMS image2DMSArray imageCubeArray

    sampler1DShadow sampler2DShadow
    sampler2DRectShadow
    sampler1DArrayShadow sampler2DArrayShadow
    samplerCubeShadow samplerCubeArrayShadow

    isampler1D isampler2D isampler3D isamplerCube isampler2DRect
    isampler1DArray isampler2DArray
    isamplerBuffer isampler2DMS isampler2DMSArray
    isamplerCubeArray

    iimage1D iimage2D iimage3D iimageCube iimage2DRect
    iimage1DArray iimage2DArray
    iimageBuffer iimage2DMS iimage2DMSArray
    iimageCubeArray

    atomic_uint
    uimage1D uimage2D uimage3D
    uimageCube uimage2DRect uimage1DArray uimage2DArray
    uimageBuffer uimage2DMS uimage2DMSArray uimageCubeArray

    usampler1D usampler2D usampler3D
    usamplerCube usampler2DRect usampler1DArray usampler2DArray
    usamplerBuffer usampler2DMS usampler2DMSArray usamplerCubeArray))

(defun an-type ()
  (optima.extra:lambda-match
    ((list op var)
     (classify :expr :type (list (analyze op) (analyze var) '())))
    ((list op var val)
     (classify :expr :type (list (analyze op) (analyze var) (analyze val))))))

(mapcar (lambda (type)
          (regist-pair-analyzer type (an-type)))
        +types+)

(defun get-type-type (node)
  (first (get-data node)))

(defun get-type-var (node)
  (second (get-data node)))

(defun get-type-val (node)
  (third (get-data node)))

(deftranslator node :type
  (let ((type (translate (get-type-type node)))
        (var (translate (get-type-var node))))
    (if (eq nil (get-type-val node))
        (format nil "~a ~a" type var)
        (format nil "~a ~a = ~a" type var (translate (get-type-val node))))))

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

(defun translate-args (args)
  (string-join (mapcar #'translate args) ", "))

(defun translate-body-node (node)
  (let ((str (translate node)))
    (format nil "~A~A~%" str (if (eq (get-tag node) :expr) ";" ""))))

(defun translate-body (body)
  (string-join (mapcar #'translate-body-node body) ""))

(deftranslator node :defun
  (let* ((type (string-downcase (get-fun-type node)))
         (name (string-downcase (get-fun-name node)))
         (args (translate-args (get-fun-args node)))
         (body (translate-body (get-fun-body node))))
    (format nil "~a ~a (~a) {~%~a}~%" type name args body)))

(named-readtables:in-readtable :standard)
