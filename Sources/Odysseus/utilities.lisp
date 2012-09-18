;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-utilities)

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
(5am:def-suite odysseus-compiler-suite
  :in odysseus-suite
  :description "Tests for the compiler.")

#+5am
(5am:def-suite odysseus-builtins-suite
  :in odysseus-suite
  :description "Tests for the built-in predicates.")
