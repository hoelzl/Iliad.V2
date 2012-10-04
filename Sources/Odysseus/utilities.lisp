;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Names
;;; =====

(defclass name-mixin ()
  ((name
    :accessor name :initarg :name :initform :<unnamed> :type symbol
    :documentation "The name of the entity that inherits this mixin."))
  (:documentation
   "Mixin inherited by all classes that have names."))

(defclass required-name-mixin (name-mixin)
  ((name
    :initform (required-argument :name)
    :documentation "The name of the entity that inherits this mixin."))
  (:documentation
   "Mixin inherited by all classes that require a name."))

;;; Errors
;;; ======

;;; Define the RUNTIME-ERROR class here so that all other packages can
;;; derive from it.

(define-condition runtime-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "A runtime error occurred."))))

(define-condition invalid-class (runtime-error)
  ((expected-class :accessor expected-class :initarg :expected-class
                   :initform (required-argument :expected-class))
   (current-class :accessor current-class :initarg :current-class
                  :initform (required-argument :current-class)))
  (:report (lambda (condition stream)
             (format stream "~A is not an instance of ~A"
                     (class-name (current-class condition))
                     (class-name (expected-class condition))))))

(define-condition incompatible-sort-declarations (runtime-error)
  ((thing :initarg :thing)
   (sort-1 :initarg :sort-1)
   (sort-2 :initarg :sort-2))
  (:report (lambda (condition stream)
             (with-slots (thing sort-1 sort-2) condition
               (format stream "Incompatible sort declarations for ~W: ~W, ~W."
                       thing sort-1 sort-2)))))


;;; Tracing
;;; -------

(defvar *trace-odysseus* t)

(defun trace-odysseus-p ()
  *trace-odysseus*)

(defun trace-odysseus ()
  (setf *trace-odysseus* t))

(defun untrace-odysseus ()
  (setf *trace-odysseus* nil))


;;; General utilities
;;; =================

(defmacro defglobal (name value &optional doc)
  `(#+sbcl sb-ext:defglobal
    #-sbcl defvar
    ,name ,value ,@(if doc (list doc) ())))

(defmacro gethash* (key hash-table default-value)
  (once-only (key hash-table)
    `(multiple-value-bind (value key-present-p)
	 (gethash ,key ,hash-table nil)
       (if (not key-present-p)
	   (let ((default ,default-value))
	     (setf (gethash ,key ,hash-table) default)
	     (values default nil))
	   (values value t)))))

(defun unquote (thing)
  (if (and (consp thing) (eql (first thing) 'quote))
      (second thing)
      thing))

(defun wrap-in-quote (thing)
  (if (or (null thing) (numberp thing) (keywordp thing) (stringp thing))
      thing
      (list 'quote thing)))

(defun wrap-in-forall (variables term)
  (if variables
      (list 'forall variables term)
      term))

;;; Helper Methods for the MOP
;;; ==========================

(defun define-method (generic-function-name
                      &key (qualifiers '()) specializers lambda-list body)
  (let* ((gf (ensure-generic-function generic-function-name))
         (method-class (c2mop:generic-function-method-class gf)))
    (multiple-value-bind (fun initargs)
        (c2mop:make-method-lambda gf
                                  (c2mop:class-prototype method-class)
                                  body
                                  nil)
      (add-method gf
                  (apply #'make-instance method-class
                         :qualifiers qualifiers
                         :specializers specializers
                         :lambda-list lambda-list
                         :function (compile nil fun)
                         initargs)))))

;;; Three-Valued Logic
;;; ==================

;;; There are many properties of programs that cannot be decided
;;; during compile time.  To handle them in a uniform manner we
;;; introduce a three-valued logic.

(deftype boolean3 ()
  '(member t :unknown nil))

(defun and3 (&rest args)
  (let ((top-result t))
    (mapc (lambda (arg)
            (cond ((not arg)
                   (return-from and3 nil))
                  ((eql arg :unknown)
                   (setf top-result :unknown))))
          args)
    top-result))

(defun or3 (&rest args)
  (let ((bottom-result nil))
    (mapc (lambda (arg)
            (cond ((eql arg :unknown)
                   (setf bottom-result :unknown))
                  (arg
                   (return-from or3 t))))
          args)
    bottom-result))

;;; Testing
;;; =======


#+5am
(5am:def-suite odysseus-suite
  :description "The suite containing all tests for Odysseus.")

#+5am
(5am:def-suite odysseus-utilities-suite
  :in odysseus-suite
  :description "Tests for utilities.")

#+5am
(5am:def-suite odysseus-macro-suite
  :in odysseus-suite
  :description "Tests for macros.")

#+5am
(5am:def-suite odysseus-syntax-suite
  :in odysseus-suite
  :description "Tests for the syntax representation.")

#+5am
(5am:def-suite odysseus-situation-suite
  :in odysseus-suite
  :description "Tests for the situations.")

#+5am
(5am:def-suite odysseus-parser-suite
  :in odysseus-suite
  :description "Tests for the parser.")

#+5am
(5am:def-suite odysseus-interpreter-suite
  :in odysseus-suite
  :description "Tests for the interpreter.")

#+5am
(5am:def-suite odysseus-compiler-suite
  :in odysseus-suite
  :description "Tests for the compiler.")

#+5am
(5am:def-suite odysseus-builtins-suite
  :in odysseus-suite
  :description "Tests for the built-in predicates.")
