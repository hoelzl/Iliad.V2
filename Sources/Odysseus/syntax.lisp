;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)
(5am:in-suite odysseus-syntax-suite)

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

(defgeneric primitive-action-definition (action-name context &optional default)
  (:documentation 
   "Returns the definition of the primitive action ACTION-NAME in CONTEXT.")
  (:method ((action-name symbol) context &optional (default nil))
    (assert context (context)
            "Cannot look up primitive-action symbol without context.")
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
    :documentation "The precondition for this action."))
  (:documentation
   "The definition of a primitive action."))

(defmethod shared-initialize :after ((self primitive-action-definition) slot-names
                                     &key context operator action-precondition)
  (assert context (context)
          "Cannot create a primitive action definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a primitive action definition without operator.")
  (setf (primitive-action-definition operator context) self)
  (when (and action-precondition (consp action-precondition)
             (or (eql slot-names t) (member 'action-precondition slot-names)))
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

(defmacro define-primitive-action (operator
                                   &key (class-name (symbolicate operator '#:-term))
                                        precondition)
  `(progn
     (defclass ,class-name (primitive-action-term)
       ())
     (defmethod operator ((term ,class-name))
       (declare (ignore term))
       ',operator)
     (defmethod declare-primitive-action
       ((operator (eql ',operator)) (context compilation-context)
        &optional (class-name ',class-name))
       (setf (primitive-action-definition operator context)
             (make-instance 'primitive-action-definition
                            :operator ',operator
                            :class class-name
                            :precondition ',precondition
                            :context context)))))



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
    ; :initform (required-argument :context)
    :documentation "The context to which this object belongs."))
  (:default-initargs :context (required-argument :context))
  (:documentation "Mixin that provides a CONTEXT slot."))


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


(defmethod shared-initialize :after ((self fluent-definition) slot-names
                                     &key context operator fluent-successor-state)
  (assert context (context)
          "Cannot create a fluent definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a fluent definition without operator.")
  (setf (fluent-definition operator context) self)
  (when (and fluent-successor-state (consp fluent-successor-state)
             (or (eql slot-names t) (member 'fluent-successor-state slot-names)))
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

(defmacro define-relational-fluent (operator
                                    &key (class-name (symbolicate operator '#:-term))
                                         successor-state)
  `(progn
     (defclass ,class-name (known-general-application-term)
       ())
     (defmethod operator ((term ,class-name))
       (declare (ignore term))
       ',operator)
     (defmethod declare-relational-fluent
       ((operator (eql ',operator)) (context compilation-context)
        &optional (class-name ',class-name))
       (setf (fluent-definition operator context)
             (make-instance 'relational-fluent-definition
                            :operator ',operator
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

(defmacro define-functional-fluent (operator
                                    &key (class-name (symbolicate operator '#:-term))
                                         successor-state)
  `(progn
     (defclass ,class-name (known-general-application-term)
       ())
     (defmethod operator ((term ,class-name))
       (declare (ignore term))
       ',operator)
     (defmethod declare-functional-fluent
       ((operator (eql ',operator)) (context compilation-context)
        &optional (class-name ',class-name))
       (setf (fluent-definition operator context)
             (make-instance 'functional-fluent-definition
                            :operator ',operator
                            :class class-name
                            :successor-state ',successor-state
                            :context context)))))

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


;;; Terms
;;; =====

;;; The class term is the superclass of all terms appearing in a parsed
;;; program representation.

(defclass term (context-mixin)
  ()
  (:documentation
   "Superclass for all terms.  Since we don't distinguish between terms,
   expressions and statements, every syntactic element inherits from this
   class."))

(defmethod shared-initialize :before ((term term) slot-names &key (intern nil) (source nil))
  "All terms accept the :source and :intern keywords."
  ;; Simply ignore the intern keyword.  If we want to signal an error when
  ;; :INTERN is true and the term cannot be interned we have to very careful
  ;; with the class hierarchy which is probably not worth the effort.
  (declare (ignore slot-names intern source))
  #+(or)
  (when intern
    (cerror "Create the instance without interning it."
	    "Trying to intern term ~A." term)))

(defgeneric source (term)
  (:documentation
    "The source form from which the term was derived, or NIL if no source is
    available")
  (:method (term)
    (declare (ignore term))
    nil))

(defgeneric to-sexpr (term)
  (:documentation
   "Convert TERM into an S-expression."))

(defclass source-mixin ()
  ((source :reader source :initarg :source :initform nil))
  (:documentation
   "Mixin for classes that provide source information"))


(defclass known-term (term)
  ()
  (:documentation
   "Inherited by all classes that are known to the compiler."))

(defgeneric is-known-term-p (term)
  (:documentation
   "Returns true if TERM is specialized in the compiler.")
  (:method (term)
    (declare (ignore term))
    nil)
  (:method ((term known-term))
    t))

(defclass variable-term (term name-mixin source-mixin)
  ((unique-name
    :initform nil
    :documentation "A unique name for code generation.")
   (is-bound-p 
    :accessor is-bound-p :initarg :is-bound-p :initform nil
    :documentation "True if the variable is bound."))
  (:documentation
   "Representation of variables."))

(defgeneric unique-name (term)
  (:documentation 
   "Return a unique name for TERM or raise an error if it has no unique name.")
  (:method (term)
    (error "Term ~A has no unique name." term))
  (:method ((term variable-term))
    (or (slot-value term 'unique-name)
        (setf (slot-value term 'unique-name)
              (gensym (if (name term) (format nil "~A" (name term)) "VAR"))))))

(define-interning-make-instance variable name)

(defmethod print-object ((term variable-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A" (name term))))

(defmethod to-sexpr ((term variable-term))
  (unique-name term))

(defun make-variable-term (name context &key (intern t) (is-bound-p nil))
  (assert (typep name 'symbol) (name)
          "~A cannot denote a variable (it is not a symbol)." name)
  (make-instance 'variable-term
                 :name name :context context :source name
                 :intern intern :is-bound-p is-bound-p))

(defclass atomic-term (term)
  ()
  (:documentation
   "Representation of atomic terms, i.e., terms that have no internal
structure."))

(defclass number-term (atomic-term)
  ((value
    :accessor value :initarg :value :initform (required-argument :value)
    :type number
    :documentation "The Lisp value of the number."))
  (:documentation
   "Representation of numbers."))

(define-interning-make-instance number value)

(defmethod source ((term number-term))
  (value term))

(defmethod to-sexpr ((term number-term))
  (value term))

;;; TODO: we need a better name for this...
;;; TODO: should primitives be interned?
(defclass primitive-term (atomic-term)
  ((value
    :accessor value :initarg :value :initform (required-argument :value)
    :documentation "The Lisp value of the thing that this term stands for"))
  (:documentation
   "Representation of primitives, i.e., non-numeric constants."))

(defmethod source ((term primitive-term))
  (value term))

(defmethod to-sexpr ((term primitive-term))
  (value term))

;;; TODO: maybe functor should not inherit from term?  Then we need to
;;; redefine the define-interning-make-instance macro.
(defclass functor-term (atomic-term required-name-mixin)
  ((arity
    :accessor arity :initarg :arity :initform (required-argument :arity)
    :type (or null non-negative-fixnum)
    :documentation "The arity of the functor.  May be NIL if the arity is not
    known, but has to be set before the functor can be interned."))
  (:documentation
   "Representation of functors.  Interned, to simplify negation."))

(define-interning-make-instance functor name arity)

(defmethod source ((term functor-term))
  (name term))

(defmethod to-sexpr ((term functor-term))
  (symbolicate (name term) "/" (arity term)))

;;; Compound Terms
;;; --------------

(defclass compound-term (term source-mixin)
  ()
  (:documentation
   "Representation of compound terms, i.e., terms that have internal
   structure."))

(defgeneric is-compound-term-p (term)
  (:documentation
   "Returns true if TERM is a compound term.")
  (:method (term)
    (declare (ignore term))
    nil)
  (:method ((term compound-term))
    t))

(defgeneric term-type-for-operator (operator context &optional default)
  (:documentation
   "Returns a symbol naming the type of a compound term with operator OPERATOR.")
  (:method ((operator symbol) context &optional (default nil))
    (assert context (context)
            "Cannot look-up operator from symbol without compilation context.")
    (gethash operator (known-operators context) default)))

(defclass application-term (compound-term)
  ()
  (:documentation
   "Representation of the application of a functor to arguments about which we
   have no further information."))

(defgeneric arguments (application-term)
  (:documentation "Return the arguments of APPLICATION-TERM."))

(defgeneric (setf arguments) (new-value application-term)
  (:documentation "Set the arguments of APPLICATION-TERM to NEW-VALUE."))

(defmethod print-object ((term application-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A" (source term))))

(defmethod to-sexpr ((term application-term))
  (list* (operator term)
         (mapcar 'to-sexpr (arguments term))))

(defclass arguments-mixin ()
  ((arguments :accessor arguments :initarg :arguments :initform '()
	      :type list))
  (:documentation "Mixin that provides an ARGUMENTS slot."))

;;; It would be necessary to define classes UNKNOWN-COMPOUND-TERM and
;;; UNKNOWN-APPLICATION-TERM in order to keep the hierarchies of known and
;;; unknown application terms consistent.  However such classes can serve no
;;; purpose, since - by definition - a term for which we can provide
;;; additional structure on top of what UNKNOWN-GENERAL-APPLICATION-TERM
;;; provides is a known term.

(defclass unknown-general-application-term
    (application-term operator-mixin arguments-mixin)
  ()
  (:documentation
   "An application term about which no additional information is known."))

(defclass known-compound-term (compound-term known-term)
  ()
  (:documentation
   "Representation of a compound term that the compiler knows about."))

(defclass known-application-term (application-term known-compound-term)
  ()
  (:documentation "A known term that is also an application term."))

(defclass unary-term (known-application-term)
  ((argument :accessor argument :initarg :argument))
  (:documentation
   "An application term that can only take a single argument."))

(defmethod shared-initialize :after ((self unary-term) slot-names
                                     &key arguments
                                          (argument nil argument-supplied-p))
  (declare (ignore slot-names argument))
  "Provide :ARGUMENTS as additional init-keyword.  Its argument must be a list
of length 1, and it must not be provided when the :ARGUMENT init-keyword is
also provided."
  (when arguments
    (assert (and (consp arguments) (= (length arguments) 1))
            (arguments)
            "Key-word argument :ARGUMENTS is not a list of length 1: ~A." arguments)
    (assert (not argument-supplied-p) ()
            "Cannot provide both :ARGUMENT and :ARGUMENTS keyword arguments.")
    (setf (argument self) (first arguments))))

(defmethod arguments ((term unary-term))
  (list (argument term)))

(defmethod (setf arguments) (new-value (term unary-term))
  (assert (and (consp new-value) (= (length new-value) 1))
          (new-value) "~A is not a list of length 1." new-value)
  (setf (argument term) (first new-value)))

(defclass binary-term (known-application-term)
  ((lhs :accessor lhs :initarg :lhs)
   (rhs :accessor rhs :initarg :rhs))
  (:documentation
   "An application term that can only take exactly two arguments."))

(defmethod shared-initialize :after ((self binary-term) slot-names
                                     &key arguments
                                          (lhs nil lhs-supplied-p)
                                          (rhs nil rhs-supplied-p))
  (declare (ignore slot-names lhs rhs))
  "Provide :ARGUMENTS as additional init-keyword.  Its argument must be a list
of length 2, and it must not be provided when either of the :LHS or :RHS
init-keywords is also provided."
  (when arguments
    (assert (and (consp arguments) (= (length arguments) 2))
            (arguments)
            "Keyword argument :ARGUMENTS is not a list of length 2: ~A." arguments)
    (assert (and (not lhs-supplied-p) (not rhs-supplied-p)) ()
            "Cannot provide both :LHS/:RHS and :ARGUMENTS keyword arguments.")
    (setf (lhs self) (first arguments)
          (rhs self) (second arguments))))

(defmethod arguments ((term binary-term))
  (list (lhs term) (rhs term)))

(defmethod (setf arguments) (new-value (term binary-term))
  (assert (and (consp new-value) (= (length new-value) 2))
          (new-value) "~A is not a list of length 2." new-value)
  (setf (lhs term) (first new-value)
        (rhs term) (second new-value)))

(defclass ternary-term (known-application-term)
  ((arg1 :accessor arg1 :initarg :arg1)
   (arg2 :accessor arg2 :initarg :arg2)
   (arg3 :accessor arg3 :initarg :arg3))
  (:documentation
   "An application term that can only take exactly three arguments."))

(defmethod shared-initialize :after ((self ternary-term) slot-names
                                     &key arguments
                                          (arg1 nil arg1-supplied-p)
                                          (arg2 nil arg2-supplied-p)
                                          (arg3 nil arg3-supplied-p))
  (declare (ignore slot-names arg1 arg2 arg3))
  "Provide :ARGUMENTS as additional init-keyword.  Its argument must be a list
of length 3, and it must not be provided when either of the :ARG1, :ARG2
or :ARG3 init-keywords is also provided."
  (when arguments
    (assert (and (consp arguments) (= (length arguments) 3))
            (arguments)
            "Keyword argument :ARGUMENTS is not a list of length 3: ~A." arguments)
    (assert (and (not arg1-supplied-p) (not arg2-supplied-p) (not arg3-supplied-p))
            ()
            "Cannot provide both :ARG1/:ARG2/:ARG3 and :ARGUMENTS keyword arguments.")
    (setf (arg1 self) (first arguments)
          (arg2 self) (second arguments)
          (arg3 self) (third arguments))))

(defmethod arguments ((term ternary-term))
  (list (arg1 term) (arg2 term) (arg3 term)))

(defmethod (setf arguments) (new-value (term ternary-term))
  (assert (and (consp new-value) (= (length new-value) 3))
          (new-value) "~A is not a list of length 3." new-value)
  (setf (arg1 term) (first new-value)
        (arg2 term) (second new-value)
        (arg3 term) (third new-value)))

(defclass known-general-application-term (known-application-term arguments-mixin)
  ()
  (:documentation
   "A knonw application term that can take an arbitrary number of arguments."))

(defclass body-term (known-compound-term)
  ;; Body initarg is not required so that we can set the body after parsing
  ;; the arguments.
  ((body
    :accessor body :initarg :body :initform '()
    :documentation "The representation of the body as a list of terms."))
  (:documentation
   "Representation of terms that have a body."))

(defmethod to-sexpr ((term body-term))
  (list* (operator term)
         (mapcar 'to-sexpr (body term))))

(defclass binding ()
  ((binding-variable 
    :accessor binding-variable :initarg :variable
    :initform (required-argument :variable))
   (binding-keywords
    :accessor binding-keywords :initarg :keywords :initform '()
    :documentation
    "A plist consisting of the keyword-value pairs provided by the binding
    declaration.  The values are in unparsed form.")
   (binding-context
    :accessor binding-context :initarg :context
    :initform (required-argument :context)
    :documentation "The context in which this binding was made.")))

(defgeneric bound-variables (binding-term)
  (:documentation
   "The variables bound by BINDING-TERM.  The value should always be equal
    to (mapcar 'binding-variable (bindings binding-term))."))

(defclass binding-term (known-compound-term)
  ((bound-variables :initform '()
    :documentation "Cache for the variables bound by that term.")
   (bindings
    :accessor bindings :initarg :bindings :initform '()
    :documentation "The binding for the term."))
  (:documentation
   "A term that binds variables.  Each subclass has to specify the scope in
   which the variables are bound."))

(defmethod bound-variables ((term binding-term))
  (or (slot-value term 'bound-variables)
      (setf (slot-value term 'bound-variables)
            (mapcar 'binding-variable (bindings term)))))


;;; Logical Compounds
;;; =================

(defclass conjunction-term (known-general-application-term)
  ()
  (:documentation
   "Representation of conjunctions."))

(defmethod operator ((term conjunction-term))
  'and)

(defclass disjunction-term (known-general-application-term)
  ()
  (:documentation
   "Representation of disjunctions."))

(defmethod operator ((term disjunction-term))
  'or)
			  
(defclass negation-term (unary-term)
  ()
  (:documentation
   "Representation of negation."))

(defmethod operator ((term negation-term))
  'not)

(defclass implication-term (binary-term)
  ((lhs :accessor premise :initarg :premise)
   (rhs :accessor consequent :initarg :consequent))
  (:documentation
   "Representation of implication"))

(defmethod operator ((term implication-term))
  'implies)

(defclass reverse-implication-term (binary-term)
  ((rhs :accessor premise :initarg :premise)
   (lhs :accessor consequent :initarg :consequent))
  (:documentation
   "Representation of implication, written the wrong way."))

(defmethod operator ((term reverse-implication-term))
  'implied-by)

(defclass equivalence-term (binary-term)
  ()
  (:documentation
   "Representation of logical equivalence (if and only if)."))

(defmethod operator ((term equivalence-term))
  'iff)

(defclass quantification-term (binding-term unary-term)
  ()
  (:documentation
   "Representation of all quantifications."))

(defmethod to-sexpr ((term quantification-term))
  (list (operator term)
        (mapcar 'to-sexpr (bound-variables term))
        (to-sexpr (argument term))))

(defclass universal-quantification-term (quantification-term)
  ()
  (:documentation
   "Representation of an universally quantified statement."))

(defmethod operator ((term universal-quantification-term))
  'foreach)

(defclass existential-quantification-term (quantification-term)
  ()
  (:documentation
   "Representation of an existentially quantified statement."))

(defmethod operator ((term existential-quantification-term))
  'exists)

(defglobal *logical-operators*
    '(and            conjunction-term
      &              conjunction-term
      |,|            conjunction-term
      or             disjunction-term
      |;|            disjunction-term
      not            negation-term
      |~|            negation-term
      ->             implication-term
      =>             implication-term
      implies        implication-term
      <-             reverse-implication-term
      <=             reverse-implication-term
      implied-by     reverse-implication-term
      is-implied-by  reverse-implication-term
      <->            equivalence-term
      <=>            equivalence-term
      iff            equivalence-term
      equiv          equivalence-term
      equivalent     equivalence-term
      is-equivalent  equivalence-term
      are-equivalent equivalence-term
      foreach        universal-quantification-term
      each           universal-quantification-term
      forall         universal-quantification-term
      exist          existential-quantification-term
      exists         existential-quantification-term))

;;; Programming-Language Terms
;;; --------------------------

;;; TODO: This can be written either nil or null.  This syntax is currently
;;; hard-coded in the parser.
(defclass empty-program-term (known-term)
  ()
  (:documentation
   "A term that represents the empty program."))

(defgeneric is-final-term-p (term)
  (:documentation
   "Returns true if TERM is final.")
  (:method (term)
    (declare (ignore term))
    nil)
  (:method ((term empty-program-term))
    t))

(defmethod to-sexpr ((term empty-program-term))
  'null)

(defclass primitive-action-term (known-general-application-term)
  ((argument :accessor action :initarg :action))
  (:documentation
   "A term describing the execution of a primitive action."))

(define-primitive-action no-operation)

(defclass test-term (unary-term)
  ((argument :accessor test :initarg :test))
  (:documentation
   "A term describing a test performed during the execution of a program."))

(defmethod operator ((term test-term))
  'holds?)

(defclass sequence-term (body-term)
  ()
  (:documentation
   "Terms that describe the execution of a sequence of action."))

(defmethod operator ((term sequence-term))
  'seq)

(defmethod is-final-term-p ((term sequence-term))
  (null (body term)))

(defclass action-choice-term (body-term)
  ()
  (:documentation
   "Terms that describe the non-deterministic choice of an action"))

(defmethod operator ((term action-choice-term))
  'one-of)

(defclass argument-choice-term (binding-term unary-term)
  ()
  (:documentation
   "Terms that describe the non-deterministic choice of an argument"))

(defmethod operator ((term argument-choice-term))
  'pick)

(defclass iteration-term (unary-term)
  ()
  (:documentation
   "Terms that describe non-deterministic iteration."))

(defmethod operator ((term iteration-term))
  'repeat)

(defclass conditional-term (ternary-term)
  ()
  (:documentation
   "Term that represents a conditional."))

(defmethod operator ((term conditional-term))
  'if)

(defclass while-loop-term (binary-term)
  ((lhs :accessor end-test :initarg :end-test)
   (rhs :accessor argument :initarg :argument))
  (:documentation
   "Term that represents a while loop."))

(defmethod operator ((term while-loop-term))
  'while)

(defclass search-term (body-term)
  ()
  (:documentation
   "Term that describes a search operation. (Has an implicit body.)"))

(defmethod operator ((term search-term))
  'search)

(defclass concurrent-term (known-general-application-term)
  ()
  (:documentation
   "Term that describes concurrent execution."))

(defmethod operator ((term concurrent-term))
  'concurrently)

(defclass prioritized-concurrent-term (known-general-application-term)
  ()
  (:documentation
   "Term that describes prioritized concurrent execution, i.e., processes
   appearing later in the argument list can only run when all earlier
   processes are blocked."))

(defmethod operator ((term prioritized-concurrent-term))
  'prioritized)

(defclass spawn-term (known-general-application-term)
  ()
  (:documentation
   "Term that describes the spawning of a new process."))

(defmethod operator ((term spawn-term))
  'spawn)

(defglobal *programming-operators*
    '(?               test-term
      test            test-term
      holds           test-term
      holds?          test-term
      seq             sequence-term
      sequentially    sequence-term
      begin           sequence-term
      progn           sequence-term
      one-of          action-choice-term
      choose          action-choice-term
      choose-action   action-choice-term
      pick            argument-choice-term
      pick-argument   argument-choice-term
      choose-argument argument-choice-term
      repeat          iteration-term
      loop            iteration-term
      iterate         iteration-term
      if              conditional-term
      while           while-loop-term
      search          search-term
      offline         search-term
      concurrently    concurrent-term
      in-parallel     concurrent-term
      prioritized     prioritized-concurrent-term
      when-blocked    prioritized-concurrent-term
      spawn           spawn-term
      new-process     spawn-term))

;;; Definition Terms
;;; ----------------

(defclass definition-term (known-compound-term)
  ()
  (:documentation
   "Terms that describe the definition of new things."))

(defclass primitive-action-definition-term (definition-term)
  ()
  (:documentation
   "Term describing the definition of a primitive action"))

(defmethod operator ((term primitive-action-definition-term))
  'define-primitive-action)

(defclass procedure-definition-term (definition-term)
  ()
  (:documentation
   "Term describing the definition of a procedure."))

(defmethod operator ((term procedure-definition-term))
  'define-procedure)

(defclass domain-definition-term (definition-term)
  ()
  (:documentation
   "Term describing the definition of a term."))

(defmethod operator ((term domain-definition-term))
  'define-domain)

(defglobal *definition-operators*
  '(define-primitive-action     primitive-action-definition-term
    defaction                   primitive-action-definition-term
    defprimitive                primitive-action-definition-term
    primitive-action            primitive-action-definition-term
    primact                     primitive-action-definition-term
    define-procedure            procedure-definition-term
    defprocedure                procedure-definition-term
    defproc                     procedure-definition-term
    procedure                   procedure-definition-term
    proc                        procedure-definition-term
    define-domain               domain-definition-term
    defdomain                   domain-definition-term))


;;; Unique Terms Mixin
;;; ==================

;;; A mixin that provides storage for an empty program term and an
;;; no-operation term.

(defclass unique-terms-mixin ()
  ((the-empty-program-term
    :initform nil
    :documentation "Storage for the empty program term.")
   (the-no-operation-term
    :initform nil
    :documentation "Storage for the no-operation term.")))

(defmethod the-empty-program-term ((self unique-terms-mixin))
  (or (slot-value self 'the-empty-program-term)
      (setf (slot-value self 'the-empty-program-term)
            (make-instance 'empty-program-term :context self))))

(defmethod the-no-operation-term ((self unique-terms-mixin))
  (or (slot-value self 'the-no-operation-term)
      (set (slot-value self 'the-no-operation-term)
           (make-instance 'no-operation-term :context self))))

;;; Compilation units
;;; =================

;;; A compilation unit is a long-lasting context.

(defvar *default-known-operators*
  (append *logical-operators* *programming-operators* *definition-operators*))

#+(or)
(unless *default-known-operators*
  (setf *default-known-operators*
        (append *logical-operators*
                *programming-operators*
                *definition-operators*)))


(defun default-known-operators ()
  (plist-hash-table *default-known-operators*))

(defclass compilation-unit (compilation-context unique-terms-mixin)
  ((known-operators
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
  (declare-primitive-action 'no-operation self))

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
