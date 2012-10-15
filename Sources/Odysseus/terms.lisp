;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))
(in-suite odysseus-syntax-suite)

;;; Local Context Mixin
;;; ===================

(defclass local-context-mixin ()
  ()
  (:documentation
   "Mixin that is inherited by terms that start a fresh scope for their free
   variables."))

(defmethod initialize-instance :around ((self local-context-mixin) &rest args &key context)
  (let ((new-context (make-instance 'local-context :enclosing-context context)))
    (remove-from-plistf args :context)
    (apply #'call-next-method self :context new-context args)))

;;; Keywords Mixin
;;; ==============

;;; A mixin for terms that support keyword arguments.

(defclass keywords-mixin ()
  ((keywords :accessor keywords :initarg :keywords
             :initform '()))
  (:documentation
   "Mixin that provides a KEYWORDS slot."))


(defgeneric declared-sort (term context)
  (:documentation
   "Returns the sort of TERM, or NIL if no sort for TERM is known.")
  (:method (term context)
    (declare (ignore context))
    (cerror "Return an empty list."
            "No declared sort for ~:W." term)
    '())
  (:method ((term keywords-mixin) context)
    (declare (ignore context))
    (getf (keywords term) :sort)))

(defgeneric (setf declared-sort) (sort term context)
  (:documentation
   "Sets the declared sort of TERM in CONTEXT to SORT."))

(defgeneric successor-state (term)
  (:documentation
   "Returns the successor state axiom of TERM, or NIL if none exists.")
  (:method ((term keywords-mixin))
    (getf (keywords term) :successor-state)))

;;; Unique Terms
;;; ============

;;; We automatically generate unique names axioms for terms that mix in
;;; TERM-WITH-UNIQUE-NAME-MIXIN.

(defclass term-with-unique-name-mixin ()
  ()
  (:documentation
   "Mixin for terms for which unique name axioms should be generated."))


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
  ;; Simply ignore the intern keyword.
  (declare (ignore slot-names intern source)))

(defgeneric source (term)
  (:documentation
    "The source form from which the term was derived, or NIL if no source is
    available")
  (:method (term)
    (declare (ignore term))
    nil))

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

(defgeneric unique-name (term)
  (:documentation 
   "Return a unique name for TERM or raise an error if it has no unique name.")
  (:method (term)
    (error "Term ~A has no unique name." term)))

(defclass variable-term (term name-mixin source-mixin)
  ((unique-name
    :accessor unique-name :initarg :unique-name
    :documentation "A unique name for code generation.")
   (declared-sort
    :initarg :sort :initform t
    :documentation
    "The declared sort of the variable or T if no sort was declared.")
   (is-bound-p 
    :accessor is-bound-p :initarg :is-bound-p :initform nil
    :documentation "True if the variable is bound.")
   (global
    :accessor global :initarg :global :initform nil
    :documentation "True if the variable is globally accessible, i.e., if
    answer terms can access this variable."))
  (:documentation
   "Representation of variables."))

(defvar *unique-variable-counter* -1)

(defun destructure-variable-name (symbol)
  (let* ((name (symbol-name symbol))
         (package (or (symbol-package symbol) *package*))
         (start-index (if (eql (aref name 0) #\?) 1 0))
         (dot-index (position #\. name))
         (real-name (intern (subseq name start-index dot-index) package))
         (sort-name (if (and dot-index (< dot-index (1- (length name))))
                        (intern (subseq name (1+ dot-index)) package)
                        t)))
    (values real-name sort-name)))

(defun make-unique-variable-name (var context)
  (declare (ignore context))
  (multiple-value-bind (var-name sort-name)
      (destructure-variable-name (name var))
    (make-symbol (format nil "?~:[VAR~;~:*~A~]~A.~A"
                         var-name
                         (incf *unique-variable-counter*)
                         sort-name))))

(defmethod declared-sort ((var variable-term) (context compilation-context))
  (declare (ignore context))
  (slot-value var 'declared-sort))

(defmethod (setf declared-sort)
    (sort (var variable-term) (context compilation-context))
  (setf (slot-value var 'declared-sort) sort)
  (setf (unique-name var) (make-unique-variable-name var context)))
        

(define-interning-make-instance variable name sort)

(defun make-variable-term (name sort context
                           &key (intern t) (is-bound-p nil) (global nil))
  (when (stringp name)
    (setf name (intern name)))
  (when (stringp sort)
    (setf sort (intern sort)))
  (assert (typep name 'symbol) (name)
          "~A cannot denote a variable (it is not a symbol)." name)
  (let ((var (make-instance 'variable-term
               :name (destructure-variable-name name)
               :sort sort
               :context context
               :source name
               :intern intern :is-bound-p is-bound-p
               :global global)))
    (setf (unique-name var)
          (make-unique-variable-name var context))
    var))

(defun make-anonymous-variable-term (sort context &key (is-bound-p nil) (global nil))
  (let* ((name (make-symbol (format nil "?_V~A.~A"
                                    (incf *unique-variable-counter*)
                                    sort)))
         (result (make-variable-term
                  name sort context
                  :is-bound-p is-bound-p :global global)))
    (setf (slot-value result 'unique-name) name)
    result))

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

(defgeneric arity (term)
  (:documentation
   "Returns the arity of TERM."))

(defgeneric (setf arity) (new-arity term)
  (:documentation
   "Sets the arity of TERM to NEW-ARITY or raises an error, if this is not
   possible."))

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

(defmethod declared-sort ((term application-term) context)
  (or (gethash (operator term) (declared-operator-sorts context))
      (call-next-method)))

(defmethod (setf declared-sort)
    (sort (term application-term) (context compilation-context))
  (declare-operator-sort (operator term) sort context))


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
  ((argument :accessor argument :initarg :argument :initform nil))
  (:documentation
   "An application term that can only take a single argument."))

(defmethod shared-initialize :after ((self unary-term) slot-names
                                     &key arguments
                                          (argument nil argument-supplied-p))
  (declare (ignore slot-names argument))
  "Provide :ARGUMENTS as additional init-keyword.  Its argument must be a list
of length 1, and it must not be provided when the :ARGUMENT init-keyword is
also provided."
  (cond
    (arguments
     (assert (and (consp arguments) (= (length arguments) 1))
             (arguments)
             "Key-word argument :ARGUMENTS is not a list of length 1: ~A." arguments)
     (assert (not argument-supplied-p) ()
             "Cannot provide both :ARGUMENT and :ARGUMENTS keyword arguments.")
     (setf (argument self) (first arguments)))))

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

(defgeneric bound-variables (binding-term)
  (:documentation
   "The variables bound by BINDING-TERM."))

(defclass binding-term (known-compound-term local-context-mixin)
  ((bound-variables
    :accessor bound-variables :initarg :bound-variables :initform '()
    :documentation "The bound variables for the term."))
  (:documentation
   "A term that binds variables.  Each subclass has to specify the scope in
   which the variables are bound."))

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

(defclass universal-quantification-term (quantification-term)
  ()
  (:documentation
   "Representation of an universally quantified statement."))

(defmethod operator ((term universal-quantification-term))
  'forall)

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
      implies        implication-term
      ->             implication-term
      =>             implication-term
      implied-by     reverse-implication-term
      <-             reverse-implication-term
      <=             reverse-implication-term
      is-implied-by  reverse-implication-term
      iff            equivalence-term
      <->            equivalence-term
      <=>            equivalence-term
      equiv          equivalence-term
      equivalent     equivalence-term
      is-equivalent  equivalence-term
      are-equivalent equivalence-term
      foreach        universal-quantification-term
      each           universal-quantification-term
      forall         universal-quantification-term
      exists          existential-quantification-term
      exist         existential-quantification-term))


;;; Mixin for Multiple Solutions
;;; ----------------------------

(defvar *default-max-solution-depth* 3)

(defclass multi-solution-mixin ()
  ((solution-depth
    :accessor solution-depth :initarg :solution-depth
    :initform 0
    :documentation
    "How many solutions of the theorem prover should be skipped before we
    accept the result.  This serves to drive search for alternative
    solutions.")
   (max-solution-depth
    :accessor max-solution-depth :initarg :max-solution-depth
    :initform *default-max-solution-depth*
    :documentation
    "The maximum solution depth for this term."))
  (:documentation
   "A mixin inherited by classes that should return multiple solutions from
   the theorem prover."))

(defgeneric clone-multi-solution-term-increasing-depth (term)
  (:documentation
   "Clone a term that implements MULTI-SOLUTION-MIXIN, and increase its
   solution depth."))


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

(defclass primitive-action-term
    (known-general-application-term multi-solution-mixin)
  ()
  (:documentation
   "A term describing the execution of a primitive action."))

(defun precondition-term (term situation)
  "Create a precondition term for TERM in SITUATION."
  (make-instance 'unknown-general-application-term
    :operator 'poss
    :arguments (list term situation)
    :context (context term)))

(defmethod clone-multi-solution-term-increasing-depth ((term primitive-action-term))
  (make-instance (class-of term)
    :arguments (arguments term)
    :solution-depth (1+ (solution-depth term))
    :max-solution-depth (max-solution-depth term)
    :context (context term)
    :source :generated-term))

(defmethod action-precondition ((term primitive-action-term))
  (let ((definition (primitive-action-definition (operator term) (context term))))
    (action-precondition definition)))

(defmethod operator ((term primitive-action-term))
  :unknown-primitive-action-term)

(define-primitive-action 'no-operation '())

(defclass test-term (unary-term keywords-mixin multi-solution-mixin)
  ()
  (:documentation
   "A term describing a test performed during the execution of a program."))

(defmethod operator ((term test-term))
  'holds?)

(defmethod clone-multi-solution-term-increasing-depth ((term test-term))
  (make-instance (class-of term)
    :argument (argument term)
    :solution-depth (1+ (solution-depth term))
    :max-solution-depth (max-solution-depth term)
    :keywords (remove-from-plist (keywords term) :solution-depth)
    :context (context term)
    :source :generated-term))


(defclass sequence-term (body-term)
  ()
  (:documentation
   "Terms that describe the execution of a sequence of action."))

(defmethod operator ((term sequence-term))
  'seq)

(defmethod is-final-term-p ((term sequence-term))
  (null (body term)))

(defclass action-choice-term (body-term multi-solution-mixin)
  ()
  (:documentation
   "Terms that describe the non-deterministic choice of an action"))

(defmethod clone-multi-solution-term-increasing-depth ((term action-choice-term))
  (make-instance (class-of term)
    :body (body term)
    :solution-depth (1+ (solution-depth term))
    :max-solution-depth (max-solution-depth term)
    :context (context term)
    :source :generated-term))

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
    '(?                  test-term
      test               test-term
      holds              test-term
      holds?             test-term
      seq                sequence-term
      sequentially       sequence-term
      begin              sequence-term
      progn              sequence-term
      one-of             action-choice-term
      choose             action-choice-term
      choose-action      action-choice-term
      pick               argument-choice-term
      pick-argument      argument-choice-term
      choose-argument    argument-choice-term
      repeat             iteration-term
      loop               iteration-term
      iterate            iteration-term
      if                 conditional-term
      while              while-loop-term
      search             search-term
      offline            search-term
      concurrently       concurrent-term
      in-parallel        concurrent-term
      prioritized        prioritized-concurrent-term
      when-blocked       prioritized-concurrent-term
      spawn              spawn-term
      new-process        spawn-term
      define-procedure   procedure-definition-term
      defprocedure       procedure-definition-term
      defproc            procedure-definition-term
      procedure          procedure-definition-term
      proc               procedure-definition-term))

;;; Declaration Terms
;;; -----------------

;;; TODO: I don't think there really is a difference between
;;; definitions and declarations.  Maybe rename the following terms to
;;; declaration terms.  Or these terms to definition terms. --tc

(defclass declaration-term (known-compound-term)
  ()
  (:documentation
   "Term that declare some logical concept."))

;;; Support for incremental redefinition of declarations.  Overwrite old
;;; declarations if the new one is equal according to DECLARE-SAME-ENTITY.

;;; TODO: Not sure whether this is a good concept right now, since the precise
;;; idea which declarations are equal turns out to be rather subtle.  Maybe
;;; it's better to blow away all declarations for the moment.  --tc
#+(or)
(defgeneric declare-same-entity (lhs rhs)
  (:documentation
   "Methods on this generic function return true if LHS and RHS are
   conceptually declarations for the same entity.  For unnamed declarations
   this means that they are syntactically the same.  For named declarations
   this means that they declare the same kind of entity with the same name,
   not that the declarations are the same."))

;;; TODO: Not sure whether this class is really needed unless we support
;;; incremental redefinition of declarations.  (But even then it's not so
;;; clear cut whether most declarations can be uniquely identified by their
;;; kind and name.  E.g., funciton declarations can oddur multiple times for
;;; the same symbol with different arities.
;;;
(defclass named-declaration-term (declaration-term name-mixin keywords-mixin)
  ()
  (:documentation
   "Declarations that have a name that identifies them."))

(defclass sort-declaration-term (named-declaration-term)
  ()
  (:documentation
   "The declaration of a sort."))

(defmethod operator ((term sort-declaration-term))
  'declare-sort)

(defmethod declared-sort ((term sort-declaration-term) context)
  (declare (ignore context))
  (name term))

(defclass subsort-declaration-term (sort-declaration-term)
  ((supersort :accessor supersort :initarg :supersort
              :initform t))
  (:documentation
   "Declares sort NAME as subsort of SUPERSORT."))

(defmethod operator ((term subsort-declaration-term))
  'declare-subsort)

(defclass sorts-incompatible-declaration-term (declaration-term)
  ((sorts :accessor sorts :initarg :sorts
          :initform '())))

(defmethod operator ((term sorts-incompatible-declaration-term))
  'declare-sorts-incompatible)

(defclass signature-declaration-term (named-declaration-term)
  ((signature :accessor signature :initarg :signature :initform '())))

(defmethod declared-sort ((term signature-declaration-term) context)
  (declare (ignore context))
  (signature term))

(defclass primitive-action-declaration-term (signature-declaration-term term-with-unique-name-mixin)
  ()
  (:documentation
   "Term describing the declaration of a primitive action."))

(defmethod operator ((term primitive-action-declaration-term))
  (declare (ignore term))
  'declare-primitive-action)

(defclass functional-fluent-declaration-term (signature-declaration-term term-with-unique-name-mixin)
  ()
  (:documentation
   "Term describing the declaration of a functional fluent."))

(defmethod operator ((term functional-fluent-declaration-term))
  (declare (ignore term))
  'declare-functional-fluent)

(defclass relational-fluent-declaration-term (signature-declaration-term)
  ()
  (:documentation
   "Term describing the declaration of a relational fluent."))

(defmethod operator ((term relational-fluent-declaration-term))
  (declare (ignore term))
  'declare-relational-fluent)

(defclass constant-declaration-term (named-declaration-term)
  ()
  (:documentation
   "Declares NAME as a constant symbol."))

(defmethod arity ((term constant-declaration-term))
  0)

(defmethod (setf arity) (new-arity (term constant-declaration-term))
  (when (not (zerop new-arity))
    (error "Cannot set the arity of a constant to ~A." new-arity)))

(defmethod operator ((term constant-declaration-term))
  'declare-constant)

(defmethod declare-constant-sort ((constant constant-declaration-term) context)
    (let* ((keys (keywords constant))
           (sort (getf keys :sort)))
      (when sort
        (let* ((sort-table (constants-for-sort-table context))
               (constants (gethash* sort sort-table  '())))
          (unless (member constant constants)
            (setf (gethash sort sort-table) 
                  (cons (make-instance 'primitive-term 
                          :value (name constant)
                          :context context
                          :source :generated-term)
                        constants)))))))

(defclass unique-constant-declaration-term (constant-declaration-term term-with-unique-name-mixin)
  ()
  (:documentation
   "Declares a constant for which unique names axioms should be generated."))

(defmethod operator ((term unique-constant-declaration-term))
  'declare-unique-constant)

(defclass arity-declaration-term (named-declaration-term)
  ((arity :accessor arity :initarg :arity
          :initform -1))
  (:documentation
   "Declaration of a term that has an arity."))


(defclass function-declaration-term (arity-declaration-term)
  ()
  (:documentation
   "Declaration of a function term."))

(defmethod operator ((term function-declaration-term))
  'declare-function)


(defclass unique-function-declaration-term (function-declaration-term term-with-unique-name-mixin)
  ())

(defmethod operator ((term unique-function-declaration-term))
  'declare-unique-function)


(defclass relation-declaration-term (arity-declaration-term)
  ()
  (:documentation
   "Declaration of a relation term."))

(defmethod operator ((term relation-declaration-term))
  'declare-relation)


(defclass ordering-declaration-term (declaration-term)
  ((ordered-symbols :accessor ordered-symbols :initarg :ordered-symbols
                    :initform '()))
  (:documentation
   "An declaration that symbols are ordered in a certain way."))

(defmethod operator ((term ordering-declaration-term))
  'declare-ordering-greaterp)

;;; Logical Assertions
;;; ------------------

 (defclass logical-sentence-declaration-term
     (declaration-term keywords-mixin local-context-mixin)
  ((sentence :accessor sentence :initarg :sentence :initform nil))
  (:documentation
   "Representation of all logical sentences that are asserted to be true."))

(defclass logical-assertion-term (logical-sentence-declaration-term)
  ())

(defmethod operator ((term logical-assertion-term))
  'assert)

(defclass logical-assumption-term (logical-sentence-declaration-term)
  ())

(defmethod operator ((term logical-assumption-term))
  'assume)

(defclass rewrite-assertion-term (logical-sentence-declaration-term)
  ())

(defmethod operator ((term rewrite-assertion-term))
  'assert-rewrite)

(defglobal *declaration-operators*
    '(declare-primitive-action    primitive-action-declaration-term
      defprimitive                primitive-action-declaration-term
      declare-functional-fluent   functional-fluent-declaration-term
      declare-relational-fluent   relational-fluent-declaration-term

      declare-sort                sort-declaration-term
      declare-subsort             subsort-declaration-term
      declare-sorts-incompatible  sorts-incompatible-declaration-term
      declare-constant            constant-declaration-term
      declare-unique-name         unique-constant-declaration-term
      declare-unique-constant     unique-constant-declaration-term
      declare-function            function-declaration-term
      declare-unique-function     unique-function-declaration-term
      declare-relation            relation-declaration-term      
      declare-ordering-greaterp   ordering-declaration-term

      assert                      logical-assertion-term
      assume                      logical-assumption-term
      assert-rewrite              rewrite-assertion-term))

;;; Definition Terms
;;; ----------------

(defclass definition-term (known-compound-term)
  ()
  (:documentation
   "Terms that describe the definition of new things."))

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
  '(define-domain               domain-definition-term
    defdomain                   domain-definition-term))

