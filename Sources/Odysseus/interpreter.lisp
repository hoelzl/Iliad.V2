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

(defgeneric can-execute-p (interpreter primitive-action-term situation)
  (:documentation
   "Returns true if it is possible to execute PRIMITIVE-ACTION-TERM in
   SITUATION, false otherwise."))

(defgeneric make-choice-point (interpreter term situation)
  (:documentation
   "Creates a new choice point for TERM in SITUATION."))

(defgeneric next-choice-point (interpreter)
  (:documentation
   "Returns the next available choice point."))

(defgeneric backtrack (interpreter)
  (:documentation
   "Abandons the current computation and continues execution of the
   interpreter at the previous choice point."))

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

(defmethod primitive-action-definition
    (action-name (interpreter interpreter) &optional default)
  (primitive-action-definition action-name (context interpreter) default))

(defmethod (setf primitive-action-definition)
    (new-value action-name (interpreter interpreter))
  (setf (primitive-action-definition action-name (context interpreter))
        new-value))

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

(defclass choice-point ()
  ((term :accessor term :initarg :term
         :initform (required-argument :term))
   (situation :accessor situation :initarg :situation
              :initform (required-argument :situation))))

(defclass basic-interpreter (interpreter)
  ((state-map
    :accessor state-map :initarg :state-map
    :initform (make-hash-table))
   (choice-points
    :accessor choice-points :initarg :choice-points
    :initform '()
    :documentation "A list of all available choice points.")
   (onlinep 
    :accessor onlinep :initarg :onlinep :initform nil
    :documentation "Returns T if the interpreter is in online mode.")))

(defvar *print-snark-timeouts* t)
(defvar *print-snark-refutations* t)

(defmethod can-execute-p
  ((interpreter basic-interpreter) (term primitive-action-term) situation)
  (let ((action-def (primitive-action-definition (operator term) interpreter)))
    (if (not (action-precondition action-def))
        t
        (let ((proof-term `(poss ,(to-sexpr term) ,(to-sexpr situation))))
          (multiple-value-bind (result reason answer)
              (prove-using-snark proof-term
                                 ;; FIXME: need to extract answer variables from term
                                 :answer '(answer ?p.person)
                                 ;; FIXME: need to provide a correct theory
                                 :set-up-theory 'set-up-theory)
            (declare (ignore answer))
            (when  (and (eql reason :refutation-found) *print-snark-refutations*)
              (format t "~&Refutation found for ~:W." proof-term))
            (when  (and (eql reason :timeout) *print-snark-timeouts*)
              (format t "~&Timeout when trying to prove ~:W." proof-term))
            result)))))

(defmethod make-choice-point
    ((interpreter basic-interpreter) term situation)
  (when (onlinep interpreter)
    (cerror "Create the choice point anyway."
            "Cannot create a choice point in online mode."))
  (push (make-instance 'choice-point :term term :situation situation)
        (choice-points interpreter)))

(defmethod next-choice-point ((interpreter basic-interpreter))
  (if (null (choice-points interpreter))
      (error "Computation failed.")
      (pop (choice-points interpreter))))

(defmethod backtrack ((interpreter basic-interpreter))
  (let ((choice-point (next-choice-point interpreter)))
    (when (onlinep interpreter)
      (cerror "Backtrack anyway."
              "Cannot backtrack to a previous choice point in online mode."))
    (interpret-1 interpreter (term choice-point) (situation choice-point))))

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

(defgeneric interpret-1 (interpreter term situation)
  (:documentation
   "Interpret a single execution step of TERM using INTERPRETER in SITUATION.
Returns the action performed by the term (an indirect instance of the class
PRIMITIVE-ACTION-TERM), the residual term that results from the evaluation
step, and the situation after executing the primitive action.  If no action
was performed by this evaluation step, an instance of NO-OPERATION-TERM is
returned as first argument."))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term list) situation)
  "Parse the program source into term representation and interpret the term."
  (interpret-1 interpreter
               (parse-into-term-representation term (context interpreter))
               situation))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term empty-program-term) situation)
  "Return a noop and another empty term (this is probably a bug)."
  (values (the-no-operation-term interpreter)
          term ;; FIXME: This should be an error term.
          situation))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term primitive-action-term) situation)
  (if (can-execute-p interpreter term situation)
      (values term
              (the-empty-program-term interpreter)
              (make-instance 'successor-situation
                             :action term
                             :previous-situation situation))
      (backtrack interpreter)))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term sequence-term) situation)
  (let ((body (body term)))
    (cond ((null body)
	   (interpret-1 interpreter (the-empty-program-term interpreter) situation))
	  ((null (rest body))
	   (interpret-1 interpreter (first body) situation))
	  (t
	   (multiple-value-bind (action rest-term new-situation)
	       (interpret-1 interpreter (first body) situation)
	     (if (is-final-term-p rest-term)
		 (values action
                         (make-instance 'sequence-term
					:context (context term)
					:body (rest body))
                         new-situation)
		 (values action
                         (make-instance 'sequence-term
					:context (context term)
					:body (cons rest-term (rest body)))
                         new-situation)))))))

;;; Interpretation
;;; ==============

;;; The following are possible variants for interpreters.

(defun skip-noops (term)
  (not (typep term 'no-operation-term)))

(defun print-everything (term)
  (declare (ignore term))
  t)

(defun interpret-and-print (term &key (interpreter (default-interpreter))
                                      (situation (make-instance 'initial-situation))
                                      (test 'skip-noops))
  (multiple-value-bind (action rest-term new-situation)
      (interpret-1 interpreter term situation)
    (when (funcall test action)
      (pprint (to-sexpr action)))
    (if (is-final-term-p rest-term)
        (to-sexpr new-situation)
        (interpret-and-print rest-term
                             :interpreter interpreter
                             :situation new-situation
                             :test test))))
