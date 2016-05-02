(in-package :parenshader)
(in-readtable :fare-quasiquote)

(defvar +multi-binops+
  '((binop-plus  . +)
    (binop-minus . -)
    (binop-mul   . *)
    (binop-div   . /)
    (binop-mod   . %)))

(defvar +multi-aliased+
  '((binop-and   . &&)
    (binop-or    . |\|\||)
    (binop-lxor  . |\^\^|)
    (binop-band  . &)
    (binop-bor   . |\||)
    (binop-xor   . |\^|)
    (binop-shl   . <<)
    (binop-shr   . >>)))

(defvar +multi-aliases+
  '((binop-and  . and)
    (binop-or   . or)
    (binop-lxor . lxor)
    (binop-band . band)
    (binop-bor  . bor)
    (binop-xor  . xor)
    (binop-shl  . <<)
    (binop-shr  . >>)))

(defvar +single-binops+
  '((>= . >=) (<= . <=) (> . >) (< . <)
    (= . ==)   (!= . !=)
    (add! . +=) (sub! . -=) (mul! . *=) (div! . /=) (mod! . %=)
    (<<!  . <<=) (>>! . >>=) (>>>! . >>>=)
    (band! . &=) (bor! . |\|=|) (xor! . |\^=|)))

(defvar +binops+ (append +multi-binops+ +multi-aliased+ +single-binops+))

(defvar +real-ops+
  (let ((ht (make-hash-table :test 'equalp)))
    (loop for (key . value) in +binops+
       do (setf (gethash (string key) ht) value))
    ht))

(defun an-binops ()
  (optima.extra:lambda-match
    ((list op a b)
     (classify :expr :binop
               (list op (string (gethash (string op) +real-ops+)))
               (list (analyze a) (analyze b))))))

(mapcar (lambda (ops)
          (regist-pair-analyzer (car ops) (an-binops)))
        +binops+)

(defun get-op-sym (node)
  (first (get-data node)))
(defun get-op-name (node)
  (second (get-data node)))
(defun get-op-args (node)
  (get-rest node))

(deftranslator node :binop
  (let ((name (get-op-name node))
        (args (mapcar #'translate (get-op-args node))))
    (format nil "(~a) ~a (~a)" (first args) name (second args))))

(named-readtables:in-readtable :standard)
