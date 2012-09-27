;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)

;;; Forward Declarations from the Parser
;;; ====================================

;;; The following declaration is introduced here to avoid compiler warnings.
;;; It logically belongs into the file parser.lisp.

;;; TODO: Maybe this is a sign that we should put all definitions of generic
;;; functions into a file that is processed early in the compilation process?

(defgeneric parse-into-term-representation (expression compilation-context)
  (:documentation
   "Parse EXPRESSION into term representation in COMPILATION-CONTEXT."))

;;; Compilation Context
;;; ===================

;;; A compilation context encapsulates the information needed by the
;;; compiler.

(defclass compilation-context ()
  ()
  (:documentation
   "Context needed by the compiler."))

(defgeneric declarations (context)
  (:documentation
   "Returns an extensible sequence of all logical declarations (sort
declations, constant and function symbol declarations, logical assertions,
etc. for this context."))

(defgeneric (setf declarations) (new-declarations context)
  (:documentation
   "Sets the logical declarations for CONTEXT to NEW-DECLARATIONS."))

(defgeneric lookup-functor (name arity context &optional create?)
  (:documentation
   "Return the functor with NAME and ARITY for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh
   functor, if CREATE? is false return NIL."))

(defgeneric (setf lookup-functor) (new-value name arity context)
  (:documentation
   "Assign NEW-VALUE as the new value of LOOKUP-FUNCTOR for NAME and ARITY."))

(defgeneric lookup-variable (name context &optional create?)
  (:documentation
   "Return the variable NAME for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh
   variable, if CREATE? is false return NIL."))

(defgeneric (setf lookup-variable) (new-value name context)
  (:documentation
   "Assign NEW-VALUE as the new value of LOOKUP-VARIABLE for NAME."))

(defgeneric lookup-number (value context &optional create?)
  (:documentation
   "Return a number constant with value VALUE for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh number
   constant, if CREATE? is false return NIL."))

(defgeneric (setf lookup-number) (new-number value context)
  (:documentation
   "Assign NEW-VALUE as the new value of LOOKUP-NUMBER for VALUE."))

(defgeneric known-operators (context)
  (:documentation
   "A hash table containing a hash table that maps known operators into their
   term type (represeted as symbol)."))

;;; TODO: Do we need this?  Should we have it?  What should its sematics be
;;; with regards to lexical/dynamic scoping?
(defgeneric (setf known-operators) (new-value context))

(defgeneric the-empty-program-term (context)
  (:documentation
   "Returns an instance of EMPTY-PROGRAM-TERM for CONTEXT that might be interned.")
  (:method ((context compilation-context))
    (make-instance 'syntax:empty-program-term :context context)))

(defgeneric the-no-operation-term (context)
  (:documentation
   "Returns an instance of NO-OPERATION-TERM for CONTEXT that might be interned.")
  (:method ((context compilation-context))
    (make-instance 'syntax:no-operation-term :context context)))   

;;; Methods for Obtaining Primitive Actions
;;; ---------------------------------------

;;; The definition of a primitive action is provided by instances of
;;; PRIMITIVE-ACTION-DEFINITION.

(defgeneric primitive-actions (context)
  (:documentation
   "A hash table containing the description of each primitive action known in
   CONTEXT."))

;;; TODO: See (setf known-operators).
(defgeneric (setf primitive-actions) (new-value context))

(defgeneric primitive-action-definition (action-name context &optional default)
  (:documentation 
   "Returns the definition of the primitive action ACTION-NAME in CONTEXT.")
  (:method ((action-name symbol) (context compilation-context)
            &optional (default nil))
    (gethash action-name (primitive-actions context) default)))

(defgeneric (setf primitive-action-definition) (new-value action-name context)
  (:documentation
   "Set the definition for primitive action ACTION-NAME in CONTEXT to
NEW-VALUE.")
  (:method (new-value (action-name symbol) context)
    (setf (gethash action-name (primitive-actions context)) new-value)))

(defclass primitive-action-definition (operator-mixin context-mixin)
  ((action-class
    :accessor action-class :initarg :class
    :initform (required-argument :class)
    :documentation "The class of this primitive action.")
   (action-precondition
    :accessor action-precondition :initarg :precondition
    :initform nil
    :documentation "The precondition for this action.")
   (action-signature
    :accessor action-signature :initarg :signature
    :initform (required-argument :signature)
    :documentation "The signature of this action."))
  (:documentation
   "The definition of a primitive action."))

(defmethod initialize-instance :after
    ((self primitive-action-definition) &key context operator action-precondition)
  (assert context (context)
          "Cannot create a primitive action definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a primitive action definition without operator.")
  (setf (primitive-action-definition operator context) self)
  (when (and action-precondition (consp action-precondition))
    (setf (slot-value self 'action-precondition)
          (parse-into-term-representation action-precondition context))))

(defgeneric declare-primitive-action (operator context &optional class-name)
  (:documentation
   "Create a new instance of PRIMITIVE-ACTION-DEFINITION and assign it as
primitive-action definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context compilation-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (setf (primitive-action-definition operator context)
          (make-instance 'primitive-action-definition
                         :operator operator :class class-name :context context))))

(defun define-primitive-action (operator signature
                                &key (class-name  (symbolicate operator '#:-term))
                                     precondition)
  (c2mop:ensure-class class-name :direct-superclasses '(primitive-action-term))
  (define-method 'operator
    :specializers (list (find-class class-name))
    :lambda-list '(term)
    :body `(lambda (term)
             (declare (ignore term))
             ',operator))
  (define-method 'declare-primitive-action
    :specializers (list (c2mop:intern-eql-specializer operator)
                        (find-class 'compilation-context))
    :lambda-list `(operator context &optional (class-name ',class-name))
    :body `(lambda (operator context &optional (class-name ',class-name))
             (setf (primitive-action-definition operator context)
                   (make-instance 'primitive-action-definition
                     :operator ',operator
                     :signature ',signature
                     :class class-name
                     :precondition ',precondition
                     :context context)))))


;;; Interaction with Snark.
;;; ======================

(defgeneric process-declaration-for-snark (declaration)
  (:documentation
   "Process DECLARATION so that the declared entity exists in Snark's
   theory.")
  (:method (declaration)
    ;; TODO: Turn this into an error once we have completed the
    ;; implementation.
    (format *error-output* "~&Don't know how to process declaration ~W.~%"
            declaration)))

(defgeneric set-up-snark (compilation-context)
  (:documentation
   "Set up Snark to prove things in COMPILATION-CONTEXT.")
  (:method ((context compilation-context))
    (iterate (for declaration in-sequence (declarations context))
      (process-declaration-for-snark declaration))
    :snark-setup-completed))
