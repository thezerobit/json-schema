#|
  This file is a part of json-schema project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage json-schema-test
  (:use :cl
        :json-schema
        :cl-test-more))
(in-package :json-schema-test)

(plan nil)

;; type
;(defparameter *type-names*
  ;(list "object" "array" "number")
  ;)
(is (validate "{}"          "{\"type\":\"object\"}") T "type: empty object")
(is (validate "{\"a\":100}" "{\"type\":\"object\"}") T "type: object")
(is (validate "[]"          "{\"type\":\"array\"}") T "type: empty array")
(is (validate "[1,2,3]"     "{\"type\":\"array\"}") T "type: array")
(is (validate "1"           "{\"type\":\"number\"}") T "type: integer as number")
(is (validate "1"           "{\"type\":\"integer\"}") T "type: integer as integer")
(is (validate "1.0"         "{\"type\":\"number\"}") T "type: float as number")
(is (validate "1.0"         "{\"type\":\"integer\"}") nil "type: float not as integer")
(is (validate "\"foo\""     "{\"type\":\"string\"}") T "type: string as string")
(is (validate "null"        "{\"type\":\"null\"}") T "type: null as null")
(is (validate "true"        "{\"type\":\"boolean\"}") T "type: true as boolean")
(is (validate "false"       "{\"type\":\"boolean\"}") T "type: false as boolean")
(mapcar (lambda (test-json)
          (is (validate test-json "{\"type\":\"any\"}") T
              (format nil "\"~a\" as any" test-json)))
        (list "\"foo\"" "{}" "[]" "1.0" "12" "null" "true" "false"))

;; properties

(finalize)
