;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-parser)
(5am:in-suite odysseus-parser-suite)

(defun starts-with-question-mark-p (symbol)
  "Returns true if SYMBOL is a symbol of length > 1 that starts with a
question mark."
  (let ((name (symbol-name symbol)))
    (and (> (length name) 1)
	 (eql (char name 0) #\?))))

;;; Handling of Declarations
;;; ========================

(defgeneric process-declaration-for-parsing (declaration context)
  (:documentation
   "Process a declaration so that the parser can use it for processing
   the rest of the program.")

  (:method ((term term) interpreter)
    (declare (ignore term interpreter))
    :do-nothing)
  ;; TODO: We might want to do something along the following lines.  For now
  ;; we use a simple implementation that always calls DEFINE-PRIMITIVE-ACTION.
  #+(or)
  (:method ((declaration primitive-action-declaration-term) context)
    (with-slots (name) declaration
      (let ((class (find-class name)))
        (if class
            (cond ((not (typep class 'primitive-action-definition))
                   (cerror "Redefine the class."
                           'invalid-class-for-action-theory-element
                           :expected-class 'primitive-action-definition
                           :current-class class)
                   (define-primitive-action (name declaration)
                       (declared-sort declaration)
                     :precondition (precondition declaration)))
                  ;; TODO: Handle case of signature changes.
                  (t
                   (define-primitive-action (name declaration)
                       (declared-sort declaration)
                     :precondition (precondition declaration))))))))
  (:method ((declaration primitive-action-declaration-term) context)
    (apply #'define-primitive-action (name declaration)
           (signature declaration)
           (keywords declaration))
    (declare-primitive-action (name declaration) context))

  (:method ((declaration functional-fluent-declaration-term) context)
    (apply #'define-functional-fluent (name declaration)
           (signature declaration)
           (keywords declaration))
    (declare-functional-fluent (name declaration) context))

  (:method ((declaration relational-fluent-declaration-term) context)
    (apply #'define-relational-fluent (name declaration)
           (signature declaration)
           (keywords declaration))
    (declare-relational-fluent (name declaration) context)))


;;; Defgeneric form for PARSE-INTO-TERM-REPRESENTATION is in syntax.lisp.

(defgeneric parse-arguments-for-term (term arguments compilation-context)
  (:documentation
   "Parse the argument list for a TERM-instance. ARGUMENTS is the
   argument list without the leading symbol that determines TERM's
   type."))

(defmethod parse-arguments-for-term ((term application-term) arguments context)
  "Parse each argument for an application term into term representation in CONTEXT."
  (setf (arguments term)
        (mapcar (lambda (subexp)
                  (parse-into-term-representation subexp context))
                arguments)))

(defmethod parse-arguments-for-term ((term body-term) arguments context)
  "Parse each for a body term argument into term representation in CONTEXT."
  (setf (body term)
        (mapcar (lambda (subexp)
                  (parse-into-term-representation subexp context))
                arguments)))

(defgeneric parse-binding (term binding-list context)
  (:documentation
   "Parse the BINDING-LIST for a single binding of TERM in CONTEXT.
BINDING-LIST may be a singleton list containing a representation of the bound
variable, or a list consisting of a representation of the bound variable
followed by a property list of keyword-value pairs."))

(defmethod parse-binding (binding (term binding-term) context)
  "Default implementation for PARSE-BINDING, parsing the variable and
accepting all keyword arguments without parsing any of them."
  (let ((binding-variable (parse-variable-term binding context))
        ;; Maybe mix keywords-mixin into variable and save the keywords?
        #+(or)
        (keywords (if (consp binding) (rest binding) '())))
    (check-type binding-variable variable-term)
    (setf (is-bound-p binding-variable) t)
    binding-variable))

(defmethod parse-arguments-for-term :around ((term binding-term) arguments context)
  "Parse the binding list and then call the next method on the rest of the
argument list."
  (let* ((binding-list (ensure-list (first arguments)))
         (new-context (make-instance 'local-context :enclosing-context context))
         (bound-variables (mapcar (lambda (binding)
                                    (parse-binding binding term new-context))
                                  binding-list)))
    (setf (bound-variables term) bound-variables)
    (call-next-method term (rest arguments) new-context)))

(defmethod parse-arguments-for-term :after ((term declaration-term) arguments context)
  "Add TERM to the declarations of CONTEXT.  This has to happen after the
arguments are passed, otherwise the name of TERM will not be set."
  (declare (ignore arguments))
  (vector-push-extend term (declarations context))
  (process-declaration-for-parsing term context))


;; TODO: We currently simply unquote arguments that should actually be
;; evaluated by the interpreter.  This is so that we don't have to fix all
;; examples when the interpreter is complete enough.
(defmethod parse-arguments-for-term ((term named-declaration-term) arguments context)
  (setf (name term) (unquote (first arguments)))
  (setf (keywords term) (mapcar 'unquote (rest arguments))))

(defmethod parse-arguments-for-term ((term subsort-declaration-term) arguments context)
  (setf (name term) (unquote (first arguments)))
  (setf (supersort term) (unquote (second arguments)))
  (setf (keywords term) (mapcar 'unquote (cddr arguments))))

(defmethod parse-arguments-for-term
    ((term sorts-incompatible-declaration-term) arguments context)
  (setf (sorts term) (mapcar 'unquote arguments)))

(defmethod parse-arguments-for-term ((term arity-declaration-term) arguments context)
  (setf (name term) (unquote (first arguments)))
  (setf (arity term) (unquote (second arguments)))
  (setf (keywords term) (mapcar 'unquote (cddr arguments))))

(defmethod parse-arguments-for-term ((term signature-declaration-term) arguments context)
  (setf (name term) (unquote (first arguments)))
  (setf (signature term) (unquote (second arguments)))
  (setf (keywords term) (mapcar 'unquote (cddr arguments))))

(defmethod parse-arguments-for-term
    ((term ordering-declaration-term) arguments context)
  (setf (ordered-symbols term) (mapcar 'unquote arguments)))

(defmethod parse-arguments-for-term
    ((term logical-sentence-declaration-term) arguments context)
  (setf (sentence term) (unquote (first arguments)))
  (setf (keywords term) (mapcar 'unquote (rest arguments))))


(defmethod parse-into-term-representation ((term term) (context compilation-context))
  "When re-parsing an already parsed term, return it unchanged.  But only if
the new context equals the one in which it was originally parsed."
  (assert (eql (context term) context) ()
          "Cannot parse term ~W in context ~W."
          term context)
  term)

(defgeneric parse-variable-term (exp compilation-context)
  (:documentation
   "Parse EXP as a VARIABLE-TERM.")

  (:method ((exp symbol) (context compilation-context))
    (let* ((name (symbol-name exp))
           (start-index (if (eql (aref name 0) #\?) 1 0))
           (dot-index (position #\. name))
           (real-name (subseq name start-index dot-index))
           (sort-name (if (and dot-index (< dot-index (1- (length name))))
                          (subseq name (1+ dot-index))
                          "T")))
      (make-variable-term real-name sort-name context)))

  (:method ((exp cons) (context compilation-context))
    (destructuring-bind (name &key sort &allow-other-keys) exp
      (let ((var (parse-variable-term name context)))
        (when sort
          (if (and (declared-sort var)
                   (not (eql (declared-sort var) t))
                   (not (eql (declared-sort var) sort)))
              (cerror "Ignore the explicit sort declaration."
                      'incompatible-sort-declarations
                      :thing exp (declared-sort var) sort)
              (setf (declared-sort var) sort)))
        var))))

(defmethod parse-into-term-representation ((exp symbol) (context compilation-context))
  "Parse a single symbol.
If it is NIL or NULL return an empty program term.
If it starts with a question mark, make a variable for CONTEXT.
If neither of these cases apply, return a primitive term."
  (cond ((or (eql exp 'nil) (eql exp 'null))
	 (make-instance 'empty-program-term :context context :source exp))
	((starts-with-question-mark-p exp)
	 (parse-variable-term exp context))
	(t
	 (make-instance 'primitive-term
                        :value exp :context context :source exp))))

(defmethod parse-into-term-representation ((exp number) (context compilation-context))
  "Return a number term with value EXP."
  (make-instance 'number-term :value exp :context context))

(defmethod parse-into-term-representation ((exp string) (context compilation-context))
  "Return a primitive term with value EXP."
  (make-instance 'primitive-term :value exp :context context))

(defmethod parse-into-term-representation
    ((exp snark::variable) (context compilation-context))
  "Return a variable term corresponding to EXP in the current context."
  (parse-into-term-representation
   (intern (format nil "?SV~A.~A"
                   (snark::variable-number exp)
                   (snark::variable-sort exp)))
   context))

(defmethod parse-into-term-representation ((exp cons) (context compilation-context))
  "Return a TERM-instance for EXP in CONTEXT.
If the operator of EXP has a known term type make an instance of that type.
Otherwise, if the operator is a primitive action, then return an instance of
that action's class.
Otherwise, if the operator is a fluent return an instance of the fluent's
class.
Otherwise return an instance of UNKNOWN-GENERAL-APPLICATION-TERM."
  (let* ((operator (first exp))
	 (known-type (term-type-for-operator operator context nil))
         (term (cond (known-type
                      (make-instance known-type :context context :source exp))
                     ((let ((primitive-action-definition
                              (gethash operator (primitive-actions context) nil)))
                        (if primitive-action-definition
                            (make-instance (action-class primitive-action-definition)
                                           :context context :source exp)
                            nil)))
                     ((let ((fluent-definition
                              (gethash operator (fluents context) nil)))
                        (if fluent-definition
                            (make-instance (fluent-class fluent-definition)
                                           :context context :source exp)
                            nil)))
                     (t
                      (make-instance 'unknown-general-application-term
                                     :operator operator :context context :source exp)))))
    (parse-arguments-for-term term (rest exp) context)
    term))
