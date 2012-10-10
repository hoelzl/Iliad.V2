;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Interpreters
;;; ============

;;; We define a class INTERPRETER from which all interpreters inherit, and a
;;; class BASIC-INTERPRETER that implements the state management functions
;;; needed by most interpreters.

;;; The Class INTERPRETER
;;; ---------------------

(defclass interpreter ()
  ((interpreter-uuid
    :accessor interpreter-uuid :initarg :uuid
    :initform (make-uuid-symbol)))
  (:documentation
   "The base class of all interpreters."))


(defgeneric superordinate-interpreter (interpreter)
  (:documentation
   "A superordinate interpreter that uses INTERPRETER for various tasks.  This
   is used to return control to the superordinate interpreter for action that
   INTERPRETER can't handle itself.")
  (:method ((interpreter interpreter))
    nil))


(defgeneric reset-interpreter (interpreter)
  (:documentation
   "Resets the state of INTERPRETER by clearing all primitive actions,
   fluents, etc.")
  (:method ((interpreter interpreter))
    (setf (context interpreter)
          (make-instance (class-of (context interpreter))))))


(define-condition cannot-interpret-term (runtime-error)
  ((interpreter :initarg :interpreter)
   (term :initarg :term))
  (:report (lambda (condition stream)
             (with-slots (interpreter term) condition
                 (format stream "Don't know how to interpret ~:W using ~A."
                         term interpreter)))))

(defgeneric interpret-1 (interpreter term situation)
  (:documentation
   "Interpret a single execution step of TERM using INTERPRETER in SITUATION.
Returns five values: 

* An indication of the action performed by TERM, either an indirect instance
  of the class PRIMITIVE-ACTION-TERM or NIL if the execution of TERM in
  SITUATION is not possible.  If no action was performed by this evaluation
  step, an instance of NO-OPERATION-TERM is returned as first argument.

* The situation after executing the primitive action returned as first
  argument or NIL if the first argument is NIL.

* A SUBSTITUTION for the variables obtained by the constraint solver.

* A deferred proof or NIL if either no proof was needed or if the proof was
  successful.

* A CONTINUATION-GENERATOR that can generate all continuations resuming this
  computation.")

  (:method :around ((interpreter interpreter) (term term) situation)
    "A method for testing purposes; checks that we actually return the right
number and types of arguments.."
    (declare (ignore situation))
    (multiple-value-bind
          (action situation substitution deferred-proof continuation-generator)
        (call-next-method)
      (check-type action (or null primitive-action-term))
      (check-type situation (or null situation))
      (check-type substitution substitution)
      (check-type deferred-proof (or null term))
      (check-type continuation-generator continuation-generator)
      (values action
              situation
              substitution
              deferred-proof
              continuation-generator)))

  (:method ((interpreter interpreter) (term term) situation)
    (let ((superordinate (superordinate-interpreter interpreter)))
      (if superordinate
          (interpret-1 superordinate term situation)
          (error 'cannot-interpret-term
                 :interpreter interpreter
                 :term term))))

  (:method ((interpreter interpreter) (term list) situation)
    "Parse the program source into term representation and interpret the
     term."
    (interpret-1 interpreter
                 (parse-into-term-representation term (context interpreter))
                 situation))

  (:method ((interpreter interpreter) (term symbol) situation)
    "Parse the program source into term representation and interpret the
     term."
    (interpret-1 interpreter
                 (parse-into-term-representation term (context interpreter))
                 situation)))


(defgeneric prove (interpreter term
                   &key solution-depth quantification-function)
  (:documentation
   "Try to prove or refute TERM in INTERPRETER.
SOLUTION-DEPTH is the number of calls to CLOSURE that we perform before
returning the solution.  QUANTIFICATION-FUNCTION is the funcion used to close
the free variables of the term.  This is usually UNIVERSALLY-QUANTIFY or
EXISTENTIALLY-QUANTIFY."))

(define-condition can-execute-p-not-defined (runtime-error)
  ((interpreter :initarg :interpreter)
   (term :initarg :term)
   (situation :initarg :situation))
  (:report (lambda (condition stream)
             (with-slots (interpreter term situation) condition
                 (format stream
                         "Cannot check whether ~A is executable by ~A in situation ~A."
                         term interpreter situation)))))
                     

(defgeneric can-execute-p (interpreter primitive-action-term situation
                           &key solution-depth)
  (:documentation
   "Returns true if it is possible to execute PRIMITIVE-ACTION-TERM in
   SITUATION, false otherwise.")
  (:method ((interpreter interpreter) term situation
            &key (solution-depth 0))
    (let ((superordinate (superordinate-interpreter interpreter)))
      (if superordinate
          (can-execute-p superordinate term situation
                         :solution-depth solution-depth)
          (error 'can-execute-p-not-defined
                 :interpreter interpreter
                 :term term
                 :situation situation)))))

(define-condition non-executing-interpreter-error
    (runtime-error)
  ((interpreter :initarg :interpreter))
  (:report (lambda (condition stream)
             (with-slots (interpreter) condition
               (format stream
                       "Interpreter ~A is not executing."
                       interpreter)))))

(defgeneric onlinep (interpreter)
  (:documentation
   "Returns true, if INTERPRETER is executing in online mode, false otherwise.
   Only defined for executing interpreters.")
  (:method ((interpreter interpreter))
    (if (superordinate-interpreter interpreter)
        (onlinep (superordinate-interpreter interpreter))
        (error 'non-executing-interpreter-error
               :interpreter interpreter))))

(defgeneric run-interpreter-loop (interpreter term situation)
  (:documentation
   "Run INTERPRETER until if finishes execution or fails, starting with TERM
   and in SITUATION.")
  (:method ((interpreter interpreter) term situation)
    (declare (ignore term situation))
    (error 'non-executing-interpreter-error
           :interpreter interpreter)))

(define-condition cannot-execute-primitive-action (runtime-error)
  ((interpreter :initarg :interpreter))
  (:report (lambda (condition stream)
             (with-slots (interpreter) condition
               (format stream "Interpreter ~A cannot execute primitive actions."
                       interpreter)))))

(defgeneric execute-primitive-action (interpreter term)
  (:documentation
   "Execute the primitive action TERM for INTEPRETER.")
  (:method ((interpreter interpreter)  term)
    (let ((superordinate (superordinate-interpreter interpreter)))
      (if superordinate
          (execute-primitive-action superordinate term)
          (error 'cannot-execute-primitive-action :interpreter interpreter)))))


(defvar *trace-continuation-creation* nil)

(defgeneric make-continuation (interpreter term situation deferred-proofs)
  (:documentation
   "Creates a new continuation that evaluates TERM in SITUATION.")
  (:method ((interpreter interpreter) term situation deferred-proofs)
    (when (and *trace-odysseus* *trace-continuation-creation*)
      (format t "~&Creating continuation for~28T~:W~%    Situation:~28T~:W~%"
              term situation))
    (make-instance 'continuation
      :term term
      :situation situation
      :deferred-proofs deferred-proofs)))



(defgeneric make-continuation-generator
    (interpreter term situation deferred-proofs reason answer)
  (:documentation
   "Creates a new CONTINUATION-GENERATOR.  The generator is non-empty if
   REASON indicates that new continuations may lead to different
  instantiations from repeating the previous proof, and a non-empty
   continuation generator otherwise.")
  (:method ((interpreter interpreter) term situation deferred-proofs reason answer)
    (declare (ignore interpreter term situation deferred-proofs reason answer))
    (warn "Calling default implementation of MAKE-CONTINUATION-GENERATOR.")
    (make-instance 'empty-continuation-generator)))


(defmethod print-object ((self interpreter) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A..."
            (let ((uuid (interpreter-uuid self)))
              (if (symbolp uuid)
                  (subseq (symbol-name uuid) 0 8)
                  uuid)))))


;;; Forwarded Definitions from Context
;;; ----------------------------------

(defmethod the-empty-program-term ((interpreter interpreter))
  "Return the empty program term of INTERPRETER's context."
  (the-empty-program-term (context interpreter)))

(defmethod the-no-operation-term ((interpreter interpreter))
  "Return the no-operation term of INTERPRETER's context."
  (the-no-operation-term (context interpreter)))


(defmethod primitive-action-definition
    (action-name (interpreter interpreter) &optional default)
  (primitive-action-definition action-name (context interpreter) default))

(defmethod (setf primitive-action-definition)
    (new-value action-name (interpreter interpreter))
  (setf (primitive-action-definition action-name (context interpreter))
        new-value))


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

(defclass basic-interpreter (interpreter)
  ((context :accessor context :initarg :context
            :initform (make-instance 'compilation-unit))
   (superordinate-interpreter
    :accessor superordinate-interpreter :initarg :subordinate-interpreter
    :initform nil))
  (:default-initargs :context (make-instance 'top-level-context)))


(defvar *print-snark-refutations* t)
(defvar *print-snark-undecidables* t)
(defvar *print-snark-timeouts* t)

(defmethod prove ((interpreter basic-interpreter) (proof-term term)
                  &key (solution-depth 0)
                       (quantification-function 'universally-quantify))
  ;; TODO: Handle substitutions for the bound variables, and introduce
  ;; standard bound variable NOW for the current situation.
  (multiple-value-bind (closed-term bound-variables)
      (funcall quantification-function
               proof-term (context interpreter) :global nil)
    (let ((free-variables (free-variables proof-term))
          (bound-variable-sexprs (mapcar 'to-sexpr bound-variables)))
      (multiple-value-bind (result reason answer)
          (prove-using-snark (to-sexpr closed-term :include-global nil)
                             :answer-vars bound-variable-sexprs
                             :context (context interpreter)
                             :solution-depth solution-depth)
        (when *trace-odysseus*
          (cond ((and (eql reason :refutation-found) *print-snark-refutations*)
                 (format t "~&Refutation found for:~28T~:W~%" (to-sexpr proof-term)))
                ((and (eql reason :undecidable) *print-snark-undecidables*)
                 (format t "~&Cannot decide:~28T~:W~%" (to-sexpr proof-term)))
                ((and (eql reason :timeout) *print-snark-timeouts*)
                 (format t "~&Timeout while proving:~28T~:W~%" (to-sexpr proof-term)))))
        (values result reason free-variables answer)))))


(defmethod can-execute-p
    ((interpreter basic-interpreter) (term primitive-action-term) situation
     &key (solution-depth 0))
  (if (not (action-precondition term))
      ;; Terms without precondition can always execute.
      (values t :no-precondition '() '())
      ;; TERM has a precondition.  Pass it to Snark for evaluation.
      (let ((proof-term (precondition-term term situation)))
        (prove interpreter proof-term
               :solution-depth solution-depth
               :quantification-function 'existentially-quantify))))


;;; Utilities for Single-Step Interpretation
;;; ========================================

(defun maybe-output-execution-trace-information
    (action term reason free-variables answer)
  (when *trace-odysseus*
    (if (or (eql reason :no-precondition)
            (null free-variables))
        (format t "~&~A~28T~:W~%    Reason:~28T~A~%"
                action (to-sexpr term) reason)
        (format t "~&~A~28T~:W~%    Reason:~28T~A~%    ~
                   Free Variables:~28T~:W~%    Answer:~28T~:W~%"
                action
                (to-sexpr term)
                reason
                (mapcar 'to-sexpr free-variables)
                answer))))

(defun make-substitution-for-interpreter (interpreter free-variables answer)
  "Creates a substitution for the values of FREE-VARIABLES bound in ANSWER"
  (let* ((local-context (make-instance 'local-context
                          :enclosing-context (context interpreter)))
         (new-terms
           (mapcar (lambda (exp)
                     (parse-into-term-representation exp local-context))
                   (rest answer))))
    (make-instance 'substitution
      :old-terms free-variables :new-terms new-terms)))


(defun proof-and-substitution-found
    (reason answer &key (ignore-variable-only-answers t))
  "Returns true if REASON indicates that a proof was found.  If
IGNORE-VARIABLE-ONLY-ANSWERS is true, then returns true only if a proof was
found and ANSWER contain at least one non-variable value."
  (when (and (eql reason :proof-found)
           (consp answer)
           (rest answer))
    (if ignore-variable-only-answers
        (not (every (lambda (elt)
                      (typep elt 'snark::variable))
                    (rest answer)))
        t)))


;;; Executing Interpreter
;;;

(defclass executing-interpreter (interpreter)
  ((subordinate-interpreter
    :accessor subordinate-interpreter
    :initarg :subordinate-interpreter
    :initform (make-instance 'single-threaded-interpreter))
   (skip-noops
    :accessor skip-noops :initarg :skip-noops
    :initform t)
   (onlinep
    :accessor onlinep :initarg :onlinep
    :initform t))
  (:documentation
   "An interpreter that executes actions and delegates all other activities to
   a subordinate interpreter.  Subclasses of EXECUTING-INTERPRETER need only
   override EXECUTE-PRIMITIVE ACTION."))

(defmethod initialize-instance :after ((self executing-interpreter) &key)
  (setf (superordinate-interpreter (subordinate-interpreter self))
          self))

(define-delegates executing-interpreter subordinate-interpreter
  (context (interpreter))
  (reset-interpreter (interpreter))
  (interpret-1 (interpreter term situation))
  (prove (interpreter term &key solution-depth quantification-function))
  (can-execute-p (interpreter primitive-action-term situation
                              &key solution-depth))
  (make-continuation (interpreter term situation deferred-proofs))
  (make-continuation-generator (interpreter term situation deferred-proofs reason answer))
  (the-empty-program-term (interpreter))
  (the-no-operation-term (interpreter)))

(defmethod interpret-1
    ((interpreter executing-interpreter) (term search-term) situation)
  (declare (ignore situation))
  (let ((old-onlinep (onlinep interpreter)))
    (setf (onlinep interpreter) nil)
    (multiple-value-bind (action new-situation substitution continuations)
        (call-next-method)
      (setf (onlinep interpreter) old-onlinep)
      (values action
              new-situation
              substitution
              nil
              continuations))))


;;; Printing Interpreter
;;; ====================

(defclass printing-interpreter (executing-interpreter)
  ()
  (:documentation
   "An interpreter that executes primitive action by printing them."))

(defmethod execute-primitive-action
    ((interpreter printing-interpreter) (term primitive-action-term))
  (when (and (contains-variable-p term))
    (cerror "Continue execution anyway."
            'unbound-variable-during-online-execution))
  (unless (and (skip-noops interpreter) (typep term 'no-operation-term))
    (format t "~&*** Performing Action ~28T~:W~60T**********~%"
            (to-sexpr term))))

