;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

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

(defgeneric enclosing-context (context)
  (:documentation
   "Returns the enclosing context of CONTEXT, or NIL if CONTEXT has no
   enclosing context.")
  (:method ((context compilation-context))
    (declare (ignore context))
    nil))

(defgeneric constants-for-sort-table (context)
  (:documentation
   "Return a table that contains a list of constants for each sort."))

(defgeneric declare-constant-sort (constant context)
  (:documentation
   "Declare the sort for CONSTANT in CONTEXT according to its declaration.  If
   the sort is is NIL then don't add a declaration."))

(defgeneric constants-for-sort (sort context)
  (:documentation
   "Return a list of all constants for SORT in CONTEXT.")
  (:method (sort context)
    (gethash sort (constants-for-sort-table context) '())))

(defgeneric declarations (context)
  (:documentation
   "Returns an extensible sequence of all logical declarations (sort
declations, constant and function symbol declarations, logical assertions,
etc. for this context."))

(defgeneric (setf declarations) (new-declarations context)
  (:documentation
   "Sets the logical declarations for CONTEXT to NEW-DECLARATIONS."))

(defgeneric declared-operator-sorts (context)
  (:documentation
   "Returns a hash table mapping operators to their declared sorts in
   CONTEXT."))

(defgeneric (setf declared-operator-sorts) (new-value context)
  (:documentation
   "Sets the hash table mapping operators to their declared sorts in
   CONTEXT."))

(defvar *warn-for-null-operator-sorts* t)

(defgeneric declare-operator-sort (operator sort context)
  (:documentation
   "Declare OPERATOR to have SORT in CONTEXT, i.e., subsequent calls
   to (GETHASH OPERATOR (DECLARED-OPERATOR-SORTS CONTEXT)) should return
   true.")
  (:method (operator sort context)
    (if sort
        (setf (gethash operator (declared-operator-sorts context)) sort)
        (when *warn-for-null-operator-sorts*
          (warn "No sort declaration for operator ~A." operator)))))

(defgeneric unique-terms (context)
  (:documentation
   "Returns a sequence containing all terms in CONTEXT for which unique names
   axioms should be generated."))

(defgeneric add-unique-term (term context)
  (:documentation
   "Adds a unique TERM to CONTEXT.  Has no effect if TERM is already a unique
   term for context."))

(defgeneric lookup-functor (name arity context &optional create?)
  (:documentation
   "Return the functor with NAME and ARITY for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh
   functor, if CREATE? is false return NIL."))

(defgeneric (setf lookup-functor) (new-value name arity context)
  (:documentation
   "Assign NEW-VALUE as the new value of LOOKUP-FUNCTOR for NAME and ARITY."))

(defgeneric lookup-variable (name sort context &optional create?)
  (:documentation
   "Return the variable NAME with sort SORT for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh
   variable, if CREATE? is false return NIL."))

(defgeneric (setf lookup-variable) (new-value name sort context)
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
    (make-instance 'empty-program-term :context context)))

(defgeneric the-no-operation-term (context)
  (:documentation
   "Returns an instance of NO-OPERATION-TERM for CONTEXT that might be interned.")
  (:method ((context compilation-context))
    (make-instance 'no-operation-term :context context)))

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

(define-condition no-definition-for-primitive-action
    (runtime-error)
  ((name :initarg :name)
   (context :initarg :context))
  (:report (lambda (condition stream)
             (with-slots (name context) condition 
               (format stream "No primitive action ~A in context ~:W"
                       name context)))))

;;; TODO: We probably should not have both PRIMITIVE-ACTION-DEFINITION
;;; and LOOKUP-PRIMITIVE-ACTION.
(defgeneric lookup-primitive-action (name context &optional default)
  (:documentation
   "Look up the definition of the primitive action NAME in CONTEXT.")
  (:method (name (context compilation-context)
            &optional (default nil default-supplied-p))
    (or (primitive-action-definition name context nil)
        (if default-supplied-p
            default
            (cerror "Return NIL."
                    'no-definition-for-primitive-action
                    :name name :context context)))))


;;; Operator and Context Mixins
;;; ===========================

(defgeneric operator (compound-term)
  (:documentation "The operator of COMPOUND-TERM."))

(defclass operator-mixin ()
  ((operator :accessor operator :initarg :operator
             :initform (required-argument :operator)))
  (:documentation "Mixin that provides an OPERATOR slot."))

(defgeneric context (thing)
  (:documentation "The compilation context in which THING is relevant."))

(defclass context-mixin ()
  ((context
    :accessor context :initarg :context 
    :initform (required-argument :context)
    :documentation "The context to which this object belongs."))
  (:documentation "Mixin that provides a CONTEXT slot."))

;;; Primitive Action Definitions
;;; ============================

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
    :reader action-precondition :initarg :precondition
    :initform nil
    :documentation "The precondition for this action.")
   (action-signature
    :accessor action-signature :initarg :signature
    :initform (required-argument :signature)
    :documentation "The signature of this action."))
  (:documentation
   "The definition of a primitive action."))

(defmethod initialize-instance :after
    ((self primitive-action-definition) &key context operator precondition)
  (assert context (context)
          "Cannot create a primitive action definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a primitive action definition without operator.")
  (setf (primitive-action-definition operator context) self)
  (when (and precondition (consp precondition))
    (let ((precondition-term (parse-into-term-representation
                              `(assert ',precondition) context)))
      (setf (slot-value self 'action-precondition) precondition-term))))

(defgeneric declare-primitive-action (operator context &optional class-name)
  (:documentation
   "Create a new instance of PRIMITIVE-ACTION-DEFINITION and assign it as
primitive-action definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context compilation-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (cerror "Continue anyway."
            "Declaring undefined primitive action.")
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



;;; Fluent Definitions
;;; ==================

;;; The definition of fluents is provided by (indirect) instances of
;;; FLUENT-DEFINITION.

(defgeneric fluents (context)
  (:documentation
   "A hash table containing the description of every fluent in CONTEXT."))

;;; TODO: see (setf known-operators)
(defgeneric (setf fluents) (new-value context))

(defgeneric fluent-definition (fluent-name context &optional default)
  (:documentation
   "Returns the definition of the fluent FLUENT-NAME in CONTEXT.")
  (:method ((fluent-name symbol) context &optional (default nil))
    (assert context (context)
            "Cannot look up fluent symbol without context.")
    (gethash fluent-name (fluents context) default)))

(defgeneric (setf fluent-definition) (new-value fluent-name context)
  (:documentation
   "Set the definition for fluent FLUENT-NAME in CONTEXT to NEW-VALUE.")
  (:method (new-value (fluent-name symbol) context)
    (setf (gethash fluent-name (fluents context)) new-value)))

(defclass fluent-definition (operator-mixin context-mixin)
  ((fluent-class
    :accessor fluent-class :initarg :class
    :initform (required-argument :class)
    :documentation "The class of this fluent.")
   (fluent-successor-state
    :accessor fluent-successor-state :initarg :successor-state
    :initform nil
    :documentation "The successor state axiom for this fluent."))
  (:documentation "The definition of a fluent."))


(defmethod initialize-instance :after
    ((self fluent-definition) &key context operator fluent-successor-state)
  (assert context (context)
          "Cannot create a fluent definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a fluent definition without operator.")
  (setf (fluent-definition operator context) self)
  (when (and fluent-successor-state (consp fluent-successor-state))
    (setf (slot-value self 'fluent-successor-state)
          (parse-into-term-representation fluent-successor-state context))))


(defclass relational-fluent-definition (fluent-definition)
  ()
  (:documentation "The definition of a relational fluent."))

(defgeneric declare-relational-fluent (operator context &optional class-name)
  (:documentation
   "Create a new instance of RELATIONAL-FLUENT-DEFINITION and assign it as
fluent definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context compilation-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (setf (fluent-definition operator context)
          (make-instance 'relational-fluent-definition
                         :operator operator :class class-name :context context))))

(defun define-relational-fluent (operator signature
                                 &key (class-name (symbolicate operator '#:-term))
                                      successor-state)
  (c2mop:ensure-class class-name
                      :direct-superclasses '(known-general-application-term))
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
             (setf (fluent-definition operator context)
                   (make-instance 'relational-fluent-definition
                     :operator ',operator
                     :signature ',signature
                     :class class-name
                     :successor-state ',successor-state
                     :context context)))))


(defclass functional-fluent-definition (fluent-definition)
  ()
  (:documentation "The definition of a functional fluent."))

(defgeneric declare-functional-fluent (operator context &optional class-name)
  (:documentation
   "Create a new instance of FUNCTIONAL-FLUENT-DEFINITION and assign it as
fluent definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context compilation-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (setf (fluent-definition operator context)
          (make-instance 'functional-fluent-definition
                         :operator operator :class class-name :context context))))

(defun define-functional-fluent (operator signature
                                 &key (class-name (symbolicate operator '#:-term))
                                      successor-state)
  (c2mop:ensure-class class-name
                      :direct-superclasses '(known-general-application-term))
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
             (setf (fluent-definition operator context)
                   (make-instance 'functional-fluent-definition
                     :operator ',operator
                     :signature ',signature
                     :class class-name
                     :successor-state ',successor-state
                     :context context)))))
