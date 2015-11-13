#|
  This file is a part of Parenshader project.
  Copyright (c) 2015 soma_arc
|#

(in-package :cl-user)
(defpackage parenshader-test-asd
  (:use :cl :asdf))
(in-package :parenshader-test-asd)

(defsystem parenshader-test
  :author "soma_arc"
  :license "LLGPL"
  :depends-on (:parenshader
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "parenshader"))))
  :description "Test system for parenshader"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
