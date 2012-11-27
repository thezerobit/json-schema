#|
  This file is a part of json-schema project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

#|
  Author: Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage json-schema-asd
  (:use :cl :asdf))
(in-package :json-schema-asd)

(defsystem json-schema
  :version "0.1"
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:st-json
               :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "json-schema"))))
  :description "json-schema validator"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op json-schema-test))))
