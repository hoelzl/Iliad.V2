;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defgeneric to-sexpr (term)
  (:documentation
   "Convert TERM into an S-expression.")

  (:method ((term term))
    :unreadable-term)

  (:method ((term variable-term))
    (unique-name term))

  (:method ((term number-term))
    (value term))

  (:method ((term primitive-term))
    (value term))
  
  (:method ((term functor-term))
    (symbolicate (name term) "/" (arity term)))

  (:method ((term application-term))
    (list* (operator term)
	   (mapcar 'to-sexpr (arguments term))))

  (:method ((term body-term))
    (list* (operator term)
	   (mapcar 'to-sexpr (body term))))

  (:method ((term quantification-term))
    (list (operator term)
	  (mapcar 'to-sexpr (bound-variables term))
	  (to-sexpr (argument term))))

  (:method ((term empty-program-term))
    'null)

  (:method ((term named-declaration-term))
    (list* (operator term)
           (wrap-in-quote (name term))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((term subsort-declaration-term))
    (list* (operator term)
           (wrap-in-quote (name term))
           (wrap-in-quote (supersort term))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((term sorts-incompatible-declaration-term))
    (list* (operator term)
           (mapcar #'wrap-in-quote (sorts term))))

  (:method ((term arity-declaration-term))
    (list* (operator term)
           (wrap-in-quote (name term))
           (wrap-in-quote (arity term))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((term ordering-declaration-term))
    (list* (operator term)
           (mapcar #'wrap-in-quote (ordered-symbols term))))

  (:method ((term logical-sentence-declaration-term))
    (list* (operator term)
           (wrap-in-quote (to-sexpr (sentence term)))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((situation initial-situation))
    (declare (ignore situation))
    'S0)

  (:method ((situation successor-situation))
    `(do ,(to-sexpr (action situation))
         ,(to-sexpr (previous-situation situation)))))



(defmethod print-object ((self term) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~W" (to-sexpr self))))

(defmethod print-object ((term variable-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A" (name term))))

(defmethod print-object ((term primitive-action-definition) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A~:[~;~:*~W~]" (operator term) (action-precondition term))))


(defgeneric free-variables (term)
  (:documentation
   "Returns a list containing the free variables of TERM.")

  (:method (term)
    (declare (ignore term))
    '())

  (:method ((term variable-term))
    (if (is-bound-p term)
	'()
	(list term)))

  (:method ((term application-term))
    (remove-duplicates
     (mapcan #'free-variables (arguments term))))

  (:method ((term body-term))
    (remove-duplicates
     (mapcan #'free-variables (body term))))

  (:method ((term successor-situation))
    (union (free-variables (action term))
	   (free-variables (previous-situation term)))))

(defun free-variable-sexprs (term)
  (mapcar 'to-sexpr (free-variables term)))

(defgeneric contains-variable-p (term)
  (:documentation
   "Returns true if TERM contains a variable, false otherwise.")
  
  (:method (term)
    (declare (ignore term))
    nil)

  (:method ((term variable-term))
    (declare (ignore term))
    t)

  (:method ((term application-term))
    (some 'contains-variable-p (arguments term)))

  (:method ((term body-term))
    (some 'contains-variable-p (body term)))

  (:method ((term successor-situation))
    (or (contains-variable-p (action term))
	(contains-variable-p (previous-situation term)))))

    
(defgeneric substitute-term (new-term old-term term)
  (:documentation
   "Substitute NEW-TERM for OLD-TERM in TERM.")

  (:method (new-term old-term term-or-situation)
    (if (eql old-term term-or-situation)
	new-term
	term-or-situation))

  (:method (new-term old-term (term unknown-general-application-term))
    (make-instance (class-of term)
      :operator (operator term)
      :arguments (mapcar (lambda (arg)
                           (substitute-term new-term old-term arg))
                         (arguments term))
      :context (context term)
      :source :generated-term))
  
  (:method (new-term old-term (term known-application-term))
    (make-instance (class-of term)
      :arguments (mapcar (lambda (arg)
                           (substitute-term new-term old-term arg))
                         (arguments term))
      :context (context term)
      :source :generated-term))

  (:method (new-term old-term (term body-term))
    (make-instance (class-of term)
      :body (mapcar (lambda (arg)
                      (substitute-term new-term old-term arg))
                    (body term))
      :context (context term)
      :source :generated-term))

  ;; TODO: Whether and how to substitute into definition terms? --tc

  (:method (new-term old-term (term successor-situation))
    (make-instance (class-of term)
      :action (substitute-term new-term old-term (action term))
      :previous-situation (substitute-term new-term old-term (previous-situation term)))))

(defgeneric substitute-terms (new-terms old-terms term)
  (:documentation
   "Substitute NEW-TERMS for OLD-TERMS in TERM.")

  (:method (new-terms old-terms term)
    (error "Don't know how to substitute ~:W for ~:W in ~:W."
           new-terms old-terms term))

  (:method ((new-terms list) (old-terms list) term-or-situation)
    (cond ((null new-terms)
           (assert (null old-terms) ()
                   "More old terms than new terms: ~:W."
                   old-terms)
           term-or-situation)
          (t
           (assert (not (null old-terms)) ()
                   "More new terms than old terms: ~:W.")
           (let ((new-term
                   (substitute-term (first new-terms) (first old-terms)
                                    term-or-situation)))
             (substitute-terms (rest new-terms) (rest old-terms) new-term))))))


;;; Generating Unique Names Axioms
;;; ==============================

(defgeneric variables-and-term-for-universal-quantification (term context)
  (:documentation
   "Returns a list of VARIABLE-TERMs that can be used to quantify over all
   variables of TERM in CONTEXT.")

  (:method ((term variable-term) (context compilation-context))
    (values '() (name term)))

  (:method ((term signature-declaration-term) (context compilation-context))
    ;; The first element of the signature is the return value.
    (let* ((signature (rest (signature term)))
           (vars (mapcar (lambda (sort)
                           (make-anonymous-variable-term sort context))
                         signature)))
      (values vars
              (cons (name term) vars))))

  (:method ((term constant-declaration-term) (context compilation-context))
    (values '()
            (name term)))

  (:method ((term function-declaration-term) (context compilation-context))
    (let* ((signature (declared-sort term))
           (vars (if (consp signature)
                     (mapcar (lambda (sort)
                               (make-anonymous-variable-term sort context))
                             (rest signature))
                     '())))
      (values vars
              (cons (name term) vars)))))

(defgeneric make-unique-names-axiom (lhs rhs context)
  (:documentation
   "Returns the unique names axiom for LHS and RHS in CONTEXT, i.e., an
   assertion that for all possible arguments, LHS and RHS are not equal.")

  (:method ((lhs term) (rhs term) (context compilation-context))
    (multiple-value-bind (lhs-vars lhs-term)
        (variables-and-term-for-universal-quantification lhs context)
      (multiple-value-bind (rhs-vars rhs-term)
          (variables-and-term-for-universal-quantification rhs context)
        (let ((vars (concatenate 'list lhs-vars rhs-vars)))
          (parse-into-term-representation
           `(assert
             ,(wrap-in-forall vars `(not (= ,lhs-term ,rhs-term))))
           context))))))

(defgeneric make-unique-names-axioms (compilation-context)
  (:method ((context compilation-context))
    (let* ((terms (unique-terms context))
           (length (length terms)))
      (iterate outer
        (for i from 0 below (1- length))
        (iterate
          (for j from (1+ i) below length)
          (in outer 
              (collect
                  (make-unique-names-axiom
                   (aref terms i) (aref terms j) context))))))))

;;; Interaction with Snark.
;;; ======================

(define-condition invalid-declaration-type (runtime-error)
  ((declaration :initarg :declaration
                :initform (required-argument :declaration)))
  (:report (lambda (condition stream)
             (with-slots (declaration) condition
               (format stream "~W is not a valid declaration for Snark."
                       declaration)))))

(defgeneric process-declaration-for-snark (declaration)
  (:documentation
   "Process DECLARATION so that the declared entity exists in Snark's
   theory.")
  
  (:method (declaration)
    (cerror "Continue without processing the declaration."
            'invalid-declaration-type :declaration declaration))
  (:method ((declaration sort-declaration-term))
    (apply #'snark:declare-sort (declared-sort declaration) (keywords declaration))
    :declare-sort)
  
  (:method ((declaration subsort-declaration-term))
    (apply #'snark:declare-subsort
           (declared-sort declaration) (supersort declaration) (keywords declaration))
    :declare-subsort)
  
  (:method ((declaration sorts-incompatible-declaration-term))
    (apply #'snark:declare-sorts-incompatible (sorts declaration))
    :declare-sorts-incompatible)
  
  (:method ((declaration constant-declaration-term))
    (apply #'snark:declare-constant (name declaration) (keywords declaration))
    :declare-constant)
  
  (:method ((declaration function-declaration-term))
    (apply #'snark:declare-function (name declaration) (arity declaration) (keywords declaration))
    :declare-function)
  
  (:method ((declaration relation-declaration-term))
    (apply #'snark:declare-relation (name declaration) (arity declaration) (keywords declaration))
    :declare-relation)
  
  (:method ((declaration ordering-declaration-term))
    (apply #'snark:declare-ordering-greaterp (ordered-symbols declaration))
    :declare-ordering-greaterp)
  
  (:method ((declaration logical-assertion-term))
    (apply #'snark::assert
           (to-sexpr (sentence declaration))
           (keywords declaration))
    :assert)
  
  (:method ((declaration logical-assumption-term))
    (apply #'snark:assume
           (to-sexpr (sentence declaration))
           (keywords declaration))
    :assume)
  
  (:method ((declaration rewrite-assertion-term))
    (apply #'snark:assert-rewrite
           (to-sexpr (sentence declaration))
           (keywords declaration))
    :assert-rewrite)
  
  (:method ((declaration primitive-action-declaration-term))
    (let ((signature (signature declaration))
          (keywords (remove-from-plist (keywords declaration) :precondition)))
      (apply #'snark:declare-function
             (name declaration)
             (1- (length signature))
             :sort signature
             keywords))
    :primitive-action/declare-function)
  
  (:method ((declaration relational-fluent-declaration-term))
    (let ((signature (signature declaration)))
      (apply #'snark:declare-relation
             (name declaration)
             (length signature)
             :sort signature
             (keywords declaration)))
    :fluent/declare-relation)
  
  (:method ((declaration functional-fluent-declaration-term))
    (let ((signature (signature declaration)))
      (apply #'snark:declare-function
             (name declaration)
             (1- (length signature))
             :sort signature
             (keywords declaration)))
    :fluent/declare-function))

(defgeneric set-up-snark (compilation-context)
  (:documentation
   "Set up Snark to prove things in COMPILATION-CONTEXT.")
  (:method ((context compilation-context))
    (iterate (for declaration in-sequence (declarations context))
      (process-declaration-for-snark declaration))
    (iterate (for (nil action) in-hashtable (primitive-actions context))
      (let ((precondition (action-precondition action)))
        (when precondition
          (format t "~&Processing ~A.~%" precondition)
          (process-declaration-for-snark precondition))))
    (iterate (for declaration in-sequence (make-unique-names-axioms context))
      (process-declaration-for-snark declaration))
    :snark-setup-completed))
