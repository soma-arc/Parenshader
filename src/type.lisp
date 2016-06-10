(in-package :parenshader)
(in-readtable :fare-quasiquote)

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

(defun regist-type-conv (sym)
  (let ((converted (intern (concatenate 'string
					"@"
					(symbol-name sym)))))
    (regist-pair-analyzer converted
			  (lambda (expr)
			    (let ((fn (analyze sym))
				  (args (mapcar #'analyze (cdr expr))))
			      (classify :expr :call (list fn) args))))))
(mapcar #'regist-type-conv +types+)

(named-readtables:in-readtable :standard)
