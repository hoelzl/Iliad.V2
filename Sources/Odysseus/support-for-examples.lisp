;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

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
   (set-up-function :accessor set-up-function :initarg :set-up-function
                    :initform (required-argument :set-up-function))
   (max-solution-depth :accessor max-solution-depth 
                       :initarg :max-solution-depth
                       :initform odysseus::*default-max-solution-depth*)
   (keys :accessor keys :initarg :keys
	 :initform '())
   (hidden? :accessor hidden? :initarg :hidden?
	    :initform nil)))

(defmethod initialize-instance :after ((self odysseus-example) &key name)
  (add-example name self))

(defgeneric full-source-code (example)
  (:method ((example symbol))
    (when example
      (full-source-code (find-example example))))
  (:method ((example odysseus-example))
    (append (funcall (set-up-function example))
            (list (term example)))))

(defgeneric run-example (example &optional execution-mode)
  (:documentation
   "Run EXAMPLE in EXECUTION-MODE (either :online or :offline)")

  (:method ((name symbol) &optional (execution-mode :online))
    (when name
      (run-example (find-example name) execution-mode)))

  (:method ((example odysseus-example) &optional (execution-mode :online))
    (let ((interpreter (default-interpreter)))
      (reset-interpreter interpreter t)
      (setf (onlinep interpreter) (eql execution-mode :online))
      (format t "~&Running example ~A in mode ~A.~%"
	      (name example) execution-mode)
      (format t "~&Source code: ~28T~:W~%" (term example))
      (let ((odysseus::*default-max-solution-depth* (max-solution-depth example)))
        (multiple-value-bind (result success?)
            (apply 'interpret
                   (full-source-code example)
                   :interpreter interpreter
                   (keys example))
          (format t "~&~:[Execution terminated~;Result~]:~28T~:W~2&" success? result))))))

(defun run-examples (examples &optional (execution-mode :online))
  (mapc (lambda (example)
	  (setf example (find-example example))
	  (unless (or (null example) (hidden? example))
	    (run-example example execution-mode)))
	examples))

(defun run-all-examples (&optional (execution-mode :online))
  (let* ((keys (copy-list (hash-table-keys *odysseus-examples*)))
	 (examples (sort keys (lambda (lhs rhs)
				(string< (symbol-name lhs) (symbol-name rhs))))))
    (run-examples examples execution-mode)))
