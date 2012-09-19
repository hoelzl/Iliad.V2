;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)

(defvar *odysseus-examples*
  (make-hash-table))

(defun delete-example (name)
  (remhash name *odysseus-examples*))

(defun add-example (name example)
  (setf (gethash name *odysseus-examples*) example))

(defun find-example (name)
  (gethash name *odysseus-examples* nil))

(defclass odysseus-example ()
  ((name :accessor name :initarg :name
	 :initform (required-argument :name))
   (term :accessor term :initarg :term
	 :initform (required-argument :term))
   (keys :accessor keys :initarg :keys
	 :initform '())))

(defmethod initialize-instance :after ((self odysseus-example) &key name)
  (add-example name self))

(defgeneric run-example (example &optional execution-mode)
  (:documentation
   "Run EXAMPLE in EXECUTION-MODE (either :online or :offline)")

  (:method ((name symbol) &optional (execution-mode :online))
    (when name
      (run-example (find-example name) execution-mode)))

  (:method ((example odysseus-example) &optional (execution-mode :online))
    (let ((interpreter (default-interpreter)))
      (reset-interpreter interpreter)
      (setf (onlinep interpreter) (eql execution-mode :online))
      (format t "~&Running example ~A in mode ~A.~%"
	      (name example) execution-mode)
      (format t "~&Source code: ~25T~:W~%" (term example))
      (let ((result (apply 'interpret (term example)
			   :interpreter interpreter
			   (keys example))))
	(format t "~&Result: ~:W~2&" result)))))

(defun run-examples (examples &optional (execution-mode :online))
  (mapc (lambda (example)
	  (run-example example execution-mode))
	examples))

(defun run-all-examples (&optional (execution-mode :online))
  (let* ((keys (copy-list (hash-table-keys *odysseus-examples*)))
	 (examples (sort keys (lambda (lhs rhs)
				(string< (symbol-name lhs) (symbol-name rhs))))))
    (run-examples examples execution-mode)))
