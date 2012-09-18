;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-interpreter)

;;; Interpreters
;;; ============

;;; We define a class INTERPRETER from which all interpreters inherit, and a
;;; class BASIC-INTERPRETER that implements the state management functions
;;; needed by most interpreters.

;;; The Class INTERPRETER
;;; ---------------------

(defclass interpreter (context-mixin)
  ()
  (:default-initargs :context (make-instance 'compilation-unit))
  (:documentation
   "The base class of all interpreters."))

(defgeneric reset-interpreter (interpreter)
  (:documentation
   "Resets the state of INTERPRETER so that no primitive actions, fluents,
   etc. are defined.")
  (:method ((interpreter interpreter))
    (setf (context interpreter) (make-instance 'compilation-unit))))

(defgeneric state-map (interpreter)
  (:documentation
   "Returns a map from situations to states."))

(defgeneric (setf state-map) (new-state-map interpreter)
  (:documentation
   "Updates the complete state-map of INTERPRETER."))

(defgeneric can-set-state-p (interpreter situation)
  (:documentation
   "Returns T if the state for INTERPRETER in SITUATION can be set, NIL
   otherwise.  When this method returns T it must be possible to execute
   (setf INTERPRETER SITUATION) successfully.")
  (:method ((interpreter interpreter) situation)
    (declare (ignore interpreter situation))
    nil))

(defgeneric state (interpreter situation &optional errorp)
  (:documentation
   "Returns the state of INTERPRETER in SITUATION.
If no state is available for SITUATION, STATE returns NIL if ERRORP is false,
raises an error otherwise.")
  (:method (interpreter situation &optional errorp)
    (let ((state (gethash situation (state-map interpreter) nil)))
      (or state
          (cond (errorp
                 (restart-case
                     (error "No state for situation ~A." situation)
                   (provide-state (situation)
                     :test (lambda () (can-set-state-p interpreter situation))
                     :report "Provide a state for the situation."
                     :interactive (lambda ()
                                    (format t "Enter a new situation: ")
                                    (list (eval (read))))
                     (setf (state interpreter situation) situation))))
                (t nil))))))

(defgeneric (setf state) (new-state interpreter situation)
  (:documentation
   "Sets the state for INTERPRETER in SITUATION."))

(defmethod the-empty-program-term ((interpreter interpreter))
  "Return the empty program term of INTERPRETER's context."
  (the-empty-program-term (context interpreter)))

(defmethod the-no-operation-term ((interpreter interpreter))
  "Return the no-operation term of INTERPRETER's context."
  (the-no-operation-term (context interpreter)))

(defmethod declare-primitive-action
    (operator (interpreter interpreter)
     &optional (class-name (symbolicate operator '#:-term)))
  (declare-primitive-action operator (context interpreter) class-name))

(defmethod declare-relational-fluent
  (operator (interpreter interpreter)
   &optional (class-name (symbolicate operator '#:-term)))
  (declare-relational-fluent operator (context interpreter) class-name))

(defmethod declare-functional-fluent
  (operator (interpreter interpreter)
   &optional (class-name (symbolicate operator '#:-term)))
  (declare-functional-fluent operator (context interpreter) class-name))


;;; The Class BASIC-INTERPRETER
;;; --------------------------- 
              
(defclass basic-interpreter-state (compilation-unit)
  ()
  (:documentation
   "The state of a basic interpreter."))

(defclass basic-interpreter (interpreter)
  ((state-map
    :accessor state-map :initarg :state-map
    :initform (make-hash-table))))

(defmethod can-set-state-p ((interpreter basic-interpreter) situation)
  (declare (ignore interpreter situation))
  t)

(defmethod (setf state) (new-state (interpreter basic-interpreter) situation)
  (cond ((can-set-state-p interpreter situation)
         (setf (gethash situation (state-map interpreter))
               new-state))
        (t
         (cerror "Try setting the state anyway."
                 "Cannot set situation ~A in interpreter ~A."
                 situation interpreter)
         (setf (gethash situation (state-map interpreter))
               new-state))))

;;; Default Interpreter
;;; ===================

(defvar *default-interpreter*
  (make-instance 'basic-interpreter))

(defun default-interpreter ()
  "Returns the default interpreter."
  *default-interpreter*)

(defun (setf default-interpreter) (new-interpreter)
  "Sets the default interpreter."
  (setf *default-interpreter* new-interpreter))

;;; Single-Step Interpretation
;;; ==========================

(defgeneric interpret-1 (interpreter term)
  (:documentation
   "Interpret a single execution step of TERM using INTERPRETER.
Returns the action performed by the term (an indirect instance of the class
PRIMITIVE-ACTION-TERM).  If no action was performed by this evaluation
step, an instance of NO-ACTION-TERM is returned."))

(defmethod interpret-1 ((interpreter basic-interpreter) (term list))
  (interpret-1 interpreter
               (parse-into-term-representation term (context interpreter))))

(defmethod interpret-1 ((interpreter basic-interpreter) (term empty-program-term))
  (declare (ignore interpreter))
  (values :no-action term))

(defmethod interpret-1 ((interpreter basic-interpreter) (term primitive-action-term))
  (values term
          (the-empty-program-term interpreter)))

(defmethod interpret-1 ((interpreter basic-interpreter) (term sequence-term))
  (let ((body (body term)))
    (cond ((null body)
	   (interpret-1 interpreter (the-empty-program-term interpreter)))
	  ((null (rest body))
	   (interpret-1 interpreter (first body)))
	  (t
	   (multiple-value-bind (action rest-term)
	       (interpret-1 interpreter (first body))
	     (if (is-final-term-p rest-term)
		 (values action
                         (make-instance 'sequence-term
					:context (context term)
					:body (rest body)))
		 (values action
                         (make-instance 'sequence-term
					:context (context term)
					:body (cons rest-term (rest body))))))))))

;;; Interpretation
;;; ==============


(defun skip-noops (term)
  (not (typep term 'no-operation-term)))

(defun print-everything (term)
  (declare (ignore term))
  t)

(defun interpret-and-print (term &key (interpreter (default-interpreter))
                                      (test 'skip-noops))
  (multiple-value-bind (action rest-term)
      (interpret-1 interpreter term)
    (when (funcall test action)
      (print (to-sexpr action)))
    (if (is-final-term-p rest-term)
        :done
        (interpret-and-print rest-term
                             :interpreter interpreter
                             :test test))))
