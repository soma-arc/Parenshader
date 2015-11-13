#|
  This file is a part of Parenshader project.
  Copyright (c) 2015 soma_arc
|#

#|
  Author: soma_arc
|#

(in-package :cl-user)
(defpackage parenshader-asd
  (:use :cl :asdf))
(in-package :parenshader-asd)

(defsystem parenshader
  :version "0.1"
  :author "soma_arc"
  :license "LLGPL"
  :depends-on (:optima)
  :components ((:module "src"
                :components
                ((:file "parenshader"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op parenshader-test))))
