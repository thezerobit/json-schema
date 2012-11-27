#|
  This file is a part of json-schema project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage json-schema-test-asd
  (:use :cl :asdf))
(in-package :json-schema-test-asd)

(defsystem json-schema-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:json-schema
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "json-schema"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
