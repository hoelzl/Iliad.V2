;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)


;;; Compilation units
;;; =================

;;; A compilation unit is a long-lasting context.

(defvar *default-known-operators*
  (append *logical-operators*
          *programming-operators*
          *declaration-operators*
          *definition-operators*))

#+(or)
(unless *default-known-operators*
  (setf *default-known-operators*
        (append *logical-operators*
                *programming-operators*
                *definition-operators*)))


(defun default-known-operators ()
  (plist-hash-table *default-known-operators*))

(defvar *default-primitive-action-names*
  '(no-operation))

(defun default-primitive-action-names ()
  *default-primitive-action-names*)

(defclass compilation-unit (compilation-context unique-terms-mixin)
  ((declarations 
    :accessor declarations :initarg :declarations
    :initform (make-array '(10) :adjustable t :fill-pointer 0)
    :documentation "Logical declarations for this compilation unit.")
   (known-operators
    :accessor known-operators :initarg :known-operators
    :initform (default-known-operators)
    :documentation "Special operators for this compilation unit.")
   (primitive-actions
    :accessor primitive-actions :initarg :primitive-actions
    :initform (make-hash-table)
    :documentation "Hash table mapping each primitive action to its definition.")
   (fluents
    :accessor fluents :initarg :fluents
    :initform (make-hash-table)
    :documentation "Hash table mapping each fluent to its definition.")
   (variable-hash-table
    :accessor variable-hash-table :initarg :variable-hash-table
    :initform (make-hash-table)
    :documentation "Hash table for interning variables.")
   (number-hash-table
    :accessor number-hash-table :initarg :number-hash-table
    :initform (make-hash-table)
    :documentation "Hash table for interning numbers.")
   (functor-hash-table
    :accessor functor-hash-table :initarg :functor-hash-table
    :initform (make-hash-table)
    :documentation "Hash table for interning functors"))
  (:documentation
   "A single compilation unit."))

(defmethod shared-initialize :after ((self compilation-unit) slot-names &key)
  (declare (ignore slot-names))
  (dolist (action (default-primitive-action-names))
    (declare-primitive-action action self)))

(defmethod lookup-variable (name (context compilation-unit) &optional (create? t))
  (let ((hash-table (slot-value context 'variable-hash-table)))
    (multiple-value-bind (variable exists?)
	(gethash name hash-table nil)
      (cond (exists? variable)
	    (create?
	     (setf (lookup-variable name context)
		   (make-variable-term name context :intern nil)))
	    (t nil)))))

(defmethod (setf lookup-variable) (new-value name (context compilation-unit))
  (check-type new-value variable-term)
  (let ((hash-table (slot-value context 'variable-hash-table)))
    (setf (gethash name hash-table) new-value)))

(defmethod lookup-number (value (context compilation-unit) &optional (create? t))
  (let* ((hash-table (slot-value context 'number-hash-table)))
    (multiple-value-bind (number exists?)
	(gethash value hash-table nil)
      (cond (exists? number)
	    (create?
	     (setf (lookup-number value context)
		   (make-instance
		    'number-term :value value :intern nil :context context)))
	    (t nil)))))

(defmethod (setf lookup-number) (new-value value (context compilation-unit))
  (check-type new-value number-term)
  (assert (= (value new-value) value) (new-value)
          "Trying to set number ~A to value ~A." value (value new-value))
  (let* ((hash-table (slot-value context 'number-hash-table)))
    (setf (gethash value hash-table) new-value)))


(defmethod lookup-functor (name arity (context compilation-unit) &optional (create? t))
  (let* ((hash-table-1 (slot-value context 'functor-hash-table))
	 (hash-table-2 (gethash* name hash-table-1 (make-hash-table))))
    (multiple-value-bind (functor exists?)
	(gethash arity hash-table-2 nil)
      (cond (exists? functor)
	    (create?
	     (setf (lookup-functor name arity context)
		   (make-instance
		    'functor-term :name name :arity arity
				  :intern nil :context context)))
	    (t nil)))))

(defmethod (setf lookup-functor) (new-value name arity (context compilation-unit))
  (check-type new-value functor-term)
  (let* ((hash-table-1 (slot-value context 'functor-hash-table))
	 (hash-table-2 (gethash* name hash-table-1 (make-hash-table))))
    (setf (gethash arity hash-table-2) new-value)))


;;; Local Context
;;; =============

(defclass local-context (compilation-context)
  ((outer-context :accessor outer-context :initarg :outer-context
                  :initform (required-argument :outer-context))
   (local-variables :accessor local-variables :initarg :local-variables
                    :initform '()))
  (:documentation
   "A temporary context for forms that bind variables."))

(defmethod lookup-variable (name (context local-context) &optional (create? t))
  (let ((local-binding (assoc name (local-variables context))))
    (if local-binding
        (cdr local-binding)
        (let* ((outer-context (outer-context context))
               (outer-var (lookup-variable name outer-context nil)))
          (or outer-var
              (if create?
                  (let ((var (make-variable-term name context :intern nil)))
                    (push (cons name var) (local-variables context))
                    var)
                  nil))))))

(defmethod (setf lookup-variable) (new-value name (context local-context))
  (let ((binding (assoc name (local-variables context))))
    (cond (binding
           (setf (cdr binding) new-value))
          (t
           (push (cons name new-value) (local-variables context))
           new-value))))

(defmethod lookup-number (value (context local-context) &optional (create? t))
  (lookup-number value (outer-context context) create?))

(defmethod (setf lookup-number) (new-value value (context local-context))
  (assert (= (value new-value) value) (new-value)
          "Trying to set number ~A to value ~A." value (value new-value))
  (setf (lookup-number value (outer-context context)) new-value))

(defmethod lookup-functor (name arity (context local-context) &optional (create? t))
  (lookup-functor name arity (outer-context context) create?))

(defmethod (setf lookup-functor) (new-value name arity (context local-context))
  (setf (lookup-functor name arity (outer-context context)) new-value))

(defmethod declarations ((context local-context))
  (declarations (outer-context context)))

(defmethod (setf declarations) (new-declaratiions (context local-context))
  (setf (declarations (outer-context context)) new-declaratiions))

(defmethod known-operators ((context local-context))
  (known-operators (outer-context context)))

(defmethod (setf known-operators) (new-value (context local-context))
  (setf (known-operators (outer-context context)) new-value))

(defmethod primitive-actions ((context local-context))
  (primitive-actions (outer-context context)))

(defmethod (setf primitive-actions) ((new-value list) (context local-context))
  (setf (primitive-actions (outer-context context)) new-value))

(defmethod fluents ((context local-context))
  (fluents (outer-context context)))

(defmethod (setf fluents) (new-value (context local-context))
  (setf (fluents (outer-context context)) new-value))

(defmethod the-empty-program-term ((context local-context))
  (the-empty-program-term (outer-context context)))

(defmethod the-no-operation-term ((context local-context))
  (the-no-operation-term (outer-context context)))


;;; Some utilities for interactive exploration
;;; ==========================================

(defun default-known-classes ()
  (let ((result '()))
    (doplist (op class *default-known-operators*)
      (declare (ignorable op))
      (pushnew class result))
    result))

(defun default-known-cpls ()
  (mapcar (lambda (class)
            (cons class 
                  (ignore-errors
                   #-(or ecl abcl)
                   (c2mop:class-precedence-list (find-class class))
                   #+ecl
                   (clos:class-precedence-list (find-class class))
                   #+abcl
                   (mop:class-precedence-list (find-class class)))))
          (default-known-classes)))
