#|
  This file is a part of json-schema project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage json-schema
  (:use :cl)
  (:export #:schema
           #:get-type-name
           #:load-json
           #:dump-json
           #:load-schema
           #:validate
           ))
(in-package :json-schema)

(defclass schema ()
  ((obj :initarg :obj)))

(defun %fix-key (keyname)
  (intern keyname "KEYWORD"))

(defun load-json (json-string)
  (st-json:read-json json-string))

(defun dump-json (json)
  (st-json:write-json-to-string json))

(defgeneric load-schema (json))

(defmethod load-schema ((json string))
  (make-instance 'schema :obj (load-json json)))

(defun validate (json schema &optional fail-fast)
  (when (stringp json) (setf json (load-json json)))
  (when (stringp schema) (setf schema (load-schema schema)))
  (let ((*fail-fast* fail-fast))
    (let ((errors (%validate json (slot-value schema 'obj) :schema nil)))
      (values (not errors) errors))))

(defun get-type-name (data)
  (etypecase data
    (st-json:jso "object")
    (cons "array")
    (null "array")
    (integer "integer")
    (real "number")
    (string "string")
    (keyword
      (case data
        (:null "null")
        (:true "boolean")
        (:false "boolean")))))

(defgeneric satisfies-type (actual-type type))

(defmethod satisfies-type (actual-type (type string))
  (or (string= type "any")
      (string= actual-type type)
      (and (string= type "number")
           (string= actual-type "integer"))))

(defmethod satisfies-type (actual-type (type cons))
  (some (lambda (type) (satisfies-type actual-type type)) type))

(defclass validation-error ()
  ((error-type :reader error-type :initarg :error-type)
   (path :reader path :initarg :path)
   (error-string :reader error-string :initarg :error-string)))

(defmethod print-object ((object validation-error) stream)
  (format stream "#<VALIDATION-ERROR type:\"~a\" msg:\"~a\">" (error-type object)
          (error-string object)))

(defun make-error (error-type path format-string &rest data)
  (make-instance 'validation-error
                 :error-type error-type
                 :path path
                 :error-string (apply #'format nil format-string data)
                 ))

(defparameter *fail-fast* nil)

(defgeneric %validate (data schema schema-type path))

(defmacro defvalidator (schema-name bindings &body body)
  (let ((schema-keyword (intern (string schema-name) "KEYWORD")))
    `(defmethod %validate (,(car bindings) ,(cadr bindings)
                            (schema-type (eql ,schema-keyword)) path)
       (block nil
         (let ((errors nil))
           ,@body
           errors)))))

(defmacro validation-error (format-string &rest args)
  `(progn (push (make-error schema-type path ,format-string ,@args) errors)
          (when *fail-fast* (return errors))))

(defmacro call-validator (data rule-keyword rule-body &optional add-to-path)
  `(let ((new-errors
           (%validate ,data ,rule-body ,rule-keyword 
                      ,(if add-to-path `(cons ,add-to-path path) `path))))
     (when new-errors
       (setf errors (append new-errors errors))
       (when *fail-fast* (return errors)))))

(defvalidator schema (data rule-body)
  (st-json:mapjso
    (lambda (rule-name rule-body)
      (call-validator data (%fix-key rule-name) rule-body))
    rule-body))

(defvalidator |type| (data type-desc)
  (let ((actual-type (get-type-name data)))
    (unless (satisfies-type actual-type type-desc)
      (validation-error "Value is type ~a which does not match ~a."
                        (dump-json actual-type)
                        (dump-json type-desc)))))

(defvalidator |properties| (data properties)
  (when properties
    (st-json:mapjso 
      (lambda (prop-name prop-schema)
        (multiple-value-bind (property property-exists)
            (st-json:getjso prop-name data)
          (if property-exists 
            (call-validator property :schema prop-schema prop-name)
            (when (eql (st-json:getjso "required" prop-schema) :true)
              (validation-error "Required property ~a is missing." (dump-json prop-name))))))
      properties)))

