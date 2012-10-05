;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defvar *report-to-sexpr-errors* nil)

(defun to-sexpr-maybe-error (term)
  (if *report-to-sexpr-errors*
      (error "~W cannot be converted into an s-expression." term)
      term))

(defgeneric to-sexpr (term &key as-list include-global)
  (:documentation
   "Convert TERM into an S-expression.")

  (:method (term &key as-list include-global)
    (declare (ignore as-list include-global))
    (to-sexpr-maybe-error term))

  (:method ((term term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (to-sexpr-maybe-error :unreadable-term))

  (:method ((term variable-term) &key as-list include-global)
    (if as-list
        `(,(unique-name term) :sort ,(slot-value term 'declared-sort)
          ,@(when (and include-global (global term))
              `(:global ,(global term))))
        (unique-name term)))

  (:method ((term number-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (value term))

  (:method ((term primitive-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (value term))
  
  (:method ((term functor-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (symbolicate (name term) "/" (arity term)))

  (:method ((term application-term) &key as-list include-global)
    (declare (ignore as-list))
    (list* (operator term)
	   (mapcar (lambda (term)
                     (to-sexpr term :include-global include-global))
                   (arguments term))))

  (:method ((term body-term) &key as-list include-global)
    (declare (ignore as-list))
    (list* (operator term)
	   (mapcar (lambda (term)
                     (to-sexpr term :include-global include-global))
                   (body term))))

  (:method ((term quantification-term) &key as-list include-global)
    (declare (ignore as-list))
    (list (operator term)
	  (mapcar (lambda (var)
                    (to-sexpr var :as-list t))
                  (bound-variables term))
	  (to-sexpr (argument term) :include-global include-global)))

  (:method ((term existential-quantification-term) &key as-list include-global)
    (declare (ignore as-list))
    (list (operator term)
	  (mapcar (lambda (var)
                    (to-sexpr var :as-list t :include-global t))
                  (bound-variables term))
	  (to-sexpr (argument term) :include-global include-global)))

  (:method ((term empty-program-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    'null)

  (:method ((term test-term) &key as-list include-global)
    (declare (ignore as-list))
    `(,(operator term)
      ,(to-sexpr (argument term) :include-global include-global)
      ,@(when (not (zerop (solution-depth term)))
          `(:solution-depth ,(to-sexpr (solution-depth term))))
      ,@(when (not (= *default-max-solution-depth* (max-solution-depth term)))
          `(:max-solution-depth ,(to-sexpr (max-solution-depth term))))
      ,@(remove-from-plist (keywords term) :solution-depth :max-solution-depth)))

  (:method ((term named-declaration-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (list* (operator term)
           (wrap-in-quote (name term))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((term subsort-declaration-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (list* (operator term)
           (wrap-in-quote (name term))
           (wrap-in-quote (supersort term))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((term sorts-incompatible-declaration-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (list* (operator term)
           (mapcar #'wrap-in-quote (sorts term))))

  (:method ((term arity-declaration-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (list* (operator term)
           (wrap-in-quote (name term))
           (wrap-in-quote (arity term))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((term ordering-declaration-term) &key as-list include-global)
    (declare (ignore as-list include-global))
    (list* (operator term)
           (mapcar #'wrap-in-quote (ordered-symbols term))))

  (:method ((term logical-sentence-declaration-term) &key as-list include-global)
    (declare (ignore as-list))
    (list* (operator term)
           (wrap-in-quote (to-sexpr (sentence term) :include-global include-global))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((situation initial-situation) &key as-list include-global)
    (declare (ignore situation as-list include-global))
    'S0)

  (:method ((situation successor-situation) &key as-list include-global)
    (declare (ignore as-list))
    `(do ,(to-sexpr (action situation) :include-global include-global)
         ,(to-sexpr (previous-situation situation) :include-global include-global))))



(defmethod print-object ((self term) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~:W" (to-sexpr self))))

(defmethod print-object ((term variable-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A.~A" (name term) (slot-value term 'declared-sort))))

(defmethod print-object ((term primitive-action-definition) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A~:[~;~:*~W~]" (operator term) (action-precondition term))))

(defmethod print-object ((term multi-solution-mixin) stream)
  (print-unreadable-object (term stream :type t)
    (format stream "~W~:[~; :SOLUTION-DEPTH ~:*~A~]~:[~; :MAX-SOLUTION-DEPTH ~:*~A~]"
            (if (typep term 'term)
                (to-sexpr term)
                :unknown-term)
            (when (not (zerop (solution-depth term)))
              (solution-depth term))
            (when (not (= *default-max-solution-depth* (max-solution-depth term)))
              (max-solution-depth term)))))

(defmethod print-object ((self situation) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~:W" (to-sexpr self))))

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
           #+(or)
           (assert (null old-terms) ()
                   "More old terms than new terms: ~:W."
                   old-terms)
           term-or-situation)
          (t
           #+(or)
           (assert (not (null old-terms)) ()
                   "More new terms than old terms: ~:W.")
           (let ((new-term
                   (substitute-term (first new-terms) (first old-terms)
                                    term-or-situation)))
             (substitute-terms (rest new-terms) (rest old-terms) new-term))))))


;;; Operations on Terms
;;; ===================

(defgeneric negate (term context)
  (:documentation
   "Returns a new term that is the negation of TERM.")

  (:method ((term cons) context)
    (negate (parse-into-term-representation term context)
            context))
  
  (:method ((term term) context)
    (make-instance 'negation-term
      :argument term
      :context context
      :source :generated-term))

  (:method ((term negation-term) context)
    (declare (ignore context))
    (argument term)))

;;; Conjoin and disjoin would be nicer names for the following functions, but
;;; then we would have to shadow the corresponding symbols from the Common
;;; Lisp package, which is probably not worth the effort.

(defun make-conjunction (term &rest additional-terms)
  (make-instance 'conjunction-term
    :arguments (cons term additional-terms)
    :context (context term)
    :source :generated-term))

(defun make-disjunction (term &rest additional-terms)
  (make-instance 'disjunction-term
    :arguments (cons term additional-terms)
    :context (context term)
    :source :generated-term))

(defun build-quantified-term (class term context globalp)
  (let ((context (make-instance 'local-context :enclosing-context context))
        (vars (free-variables term)))
    (if vars
        (let* ((fresh-vars (mapcar (lambda (var)
                                     (make-anonymous-variable-term
                                      (declared-sort var context) context
                                      :is-bound-p t :global globalp))
                                   vars))
               (new-body (substitute-terms fresh-vars vars term))
               (result (make-instance class
                         :bound-variables fresh-vars
                         :argument new-body
                         :context context
                         :source :generated-term)))
          (assert (null (free-variables result)) ()
                  "The universally quantified term ~W contains free variables."
                  result)
          (values result fresh-vars vars))
        (values term vars vars))))

(defgeneric universally-quantify (term context &key global)
  (:documentation
   "Returns a new term that is equivalent to TERM with all free variables
   quantified.")
  
  (:method ((term cons) context &key global)
    (universally-quantify (parse-into-term-representation term context)
                          context :global global))

  (:method ((term term) context &key global)
    (build-quantified-term 'universal-quantification-term
                           term context global)))

(defgeneric existentially-quantify (term context &key global)
  (:documentation
   "Returns a new term that is equivalent to TERM with all free variables
   quantified.")
  
  (:method ((term cons) context &key global)
    (existentially-quantify (parse-into-term-representation term context)
                            context
                            :global global))

  (:method ((term term) context &key global)
    (build-quantified-term 'existential-quantification-term
                           term context global)))

;;; Generating Unique Names Axioms
;;; ==============================

;;; TODO: This name is quite bad, since it implies something more general than
;;; it actually does.
(defgeneric variables-and-term-for-universal-quantification (term context)
  (:documentation
   "Returns a list of variable names that can be used to quantify over all
   arguments of TERM in CONTEXT.  The second argument is TERM applied to the
   variables.")

  (:method ((term cons) (context compilation-context))
    (variables-and-term-for-universal-quantification
     (parse-into-term-representation term context)
     context))

  (:method ((term variable-term) (context compilation-context))
    (values '() (name term)))

  (:method ((term application-term) (context compilation-context))
    (let* ((arguments (arguments term))
           (sort (declared-sort term context))
           (vars (mapcar (lambda (sort)
                           (unique-name (make-anonymous-variable-term sort context)))
                         (cond ((= (length arguments) (length sort))
                               sort)
                              ((= (length arguments) (1- (length sort)))
                               (rest sort))
                              (t
                               (error "Declared sort ~W and arguments ~W do not match for ~A."
                                      sort arguments term))))))
      (values vars
              (cons (operator term) vars))))

  (:method ((term signature-declaration-term) (context compilation-context))
    ;; The first element of the signature is the return value.
    (let* ((signature (rest (signature term)))
           (vars (mapcar (lambda (sort)
                           (unique-name (make-anonymous-variable-term sort context)))
                         signature)))
      (values vars
              (cons (name term) vars))))

  (:method ((term constant-declaration-term) (context compilation-context))
    (values '()
            (name term)))

  (:method ((term function-declaration-term) (context compilation-context))
    (let* ((signature (declared-sort term context))
           (vars (if (consp signature)
                     (mapcar (lambda (sort)
                               (unique-name (make-anonymous-variable-term sort context)))
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

(defgeneric make-unique-names-axiom-for-arguments (term context)
  (:documentation
   "Returns the unique names axiom for the arguments of TERM in CONTEXT, i.e.,
   an assertion that the operator of TERM is injective.")
  ;; TODO: Would it be enough to simply declare the terms as injective?
  (:method ((term term) (context compilation-context))
    (multiple-value-bind (lhs-vars lhs-term)
        (variables-and-term-for-universal-quantification term context)
      (multiple-value-bind (rhs-vars rhs-term)
          (variables-and-term-for-universal-quantification term context)
        (let* ((vars (concatenate 'list lhs-vars rhs-vars))
               (terms-unequal-assertion
                 `(or ,@(mapcar (lambda (var1 var2)
                                  `(not (= ,var1 ,var2)))
                                lhs-vars rhs-vars))))
          (parse-into-term-representation
           `(assert
             ,(wrap-in-forall 
               vars
               `(iff ,terms-unequal-assertion
                     (not (= ,lhs-term ,rhs-term)))))
           context))))))

(defgeneric make-unique-names-axioms (compilation-context)
  (:method ((context compilation-context))
    (let* ((terms (unique-terms context))
           (length (length terms)))
      (iterate outer
        (for i from 0 below (1- length))
        (iterate
          (for j from i below length)
          (in outer 
              (if (= i j)
                  (collect
                      (make-unique-names-axiom-for-arguments (aref terms j)
                                                             context))
                  (collect
                      (make-unique-names-axiom (aref terms i)
                                               (aref terms j)
                                               context)))))))))

;;; Interaction with Snark.
;;; ======================

(define-condition invalid-declaration-type (runtime-error)
  ((declaration :initarg :declaration
                :initform (required-argument :declaration)))
  (:report (lambda (condition stream)
             (with-slots (declaration) condition
               (format stream "~W is not a valid declaration for Snark."
                       declaration)))))

(defgeneric process-declaration-for-snark
    (declaration &key context rewrite-too supported)
  (:documentation
   "Process DECLARATION so that the declared entity exists in Snark's
   theory.")
  
  (:method (declaration &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (cerror "Continue without processing the declaration."
            'invalid-declaration-type :declaration declaration))

  (:method ((declaration sort-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore rewrite-too supported))
    (apply #'snark:declare-sort
           (declared-sort declaration context)
           (keywords declaration))
    :declare-sort)
  
  (:method ((declaration subsort-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore rewrite-too supported))
    (apply #'snark:declare-subsort
           (declared-sort declaration context)
           (supersort declaration)
           (keywords declaration))
    :declare-subsort)
  
  (:method ((declaration sorts-incompatible-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (apply #'snark:declare-sorts-incompatible (sorts declaration))
    :declare-sorts-incompatible)
  
  (:method ((declaration constant-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (apply #'snark:declare-constant (name declaration) (keywords declaration))
    :declare-constant)
  
  (:method ((declaration function-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (apply #'snark:declare-function (name declaration) (arity declaration)
           (keywords declaration))
    :declare-function)
  
  (:method ((declaration relation-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (apply #'snark:declare-relation (name declaration) (arity declaration)
           (keywords declaration))
    :declare-relation)
  
  (:method ((declaration ordering-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (apply #'snark:declare-ordering-greaterp (ordered-symbols declaration))
    :declare-ordering-greaterp)
  
  (:method ((declaration logical-assertion-term)
            &key context rewrite-too (supported nil))
    (declare (ignore context))
    (let ((keywords (keywords declaration)))
      (when (getf keywords :rewrite-too)
        (setf rewrite-too t))
      (remove-from-plistf keywords :rewrite-too)
      (cond ((eql supported :always)
             (setf keywords (list* :supported t
                                   (remove-from-plist keywords :supported))))
            ((eql supported :never)
             (setf keywords (list* :supported nil
                                   (remove-from-plist keywords :supported))))
            ((and supported (not (getf keywords :supported)))
             (setf keywords (list* :supported t keywords))))
      (apply #'snark::assert
             (to-sexpr (sentence declaration))
             keywords))
    (when rewrite-too
      (snark::assert-rewrite (to-sexpr (sentence declaration))))
    :assert)
  
  (:method ((declaration logical-assumption-term)
            &key context rewrite-too (supported nil))
    (declare (ignore context))
    (let ((keywords (keywords declaration)))
      (when (getf keywords :rewrite-too)
        (setf rewrite-too t))
      (remove-from-plistf keywords :rewrite-too)
      (cond ((eql supported :always)
             (setf keywords (list* :supported t
                                   (remove-from-plist keywords :supported))))
            ((eql supported :never)
             (setf keywords (list* :supported nil
                                   (remove-from-plist keywords :supported))))
            ((and supported (not (getf keywords :supported)))
             (setf keywords (list* :supported t keywords))))
      (apply #'snark:assume
             (to-sexpr (sentence declaration))
             keywords))
    (when rewrite-too
      (snark::assert-rewrite (to-sexpr (sentence declaration))))
    :assume)
  
  (:method ((declaration rewrite-assertion-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (remove-from-plistf (keywords declaration) :rewrite-too)
    (apply #'snark:assert-rewrite
           (to-sexpr (sentence declaration))
           (keywords declaration))
    :assert-rewrite)
  
  (:method ((declaration primitive-action-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (let ((signature (signature declaration))
          (keywords (remove-from-plist (keywords declaration) :precondition)))
      (apply #'snark:declare-function
             (name declaration)
             (1- (length signature))
             :sort signature
             :injective t
             keywords))
    :primitive-action/declare-function)
  
  (:method ((declaration relational-fluent-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (let ((signature (signature declaration)))
      (apply #'snark:declare-relation
             (name declaration)
             (length signature)
             :sort signature
             (keywords declaration)))
    :fluent/declare-relation)
  
  (:method ((declaration functional-fluent-declaration-term)
            &key context rewrite-too supported)
    (declare (ignore context rewrite-too supported))
    (let ((signature (signature declaration)))
      (apply #'snark:declare-function
             (name declaration)
             (1- (length signature))
             :sort signature
             :injective t
             (keywords declaration)))
    :fluent/declare-function))

(defvar *trace-declaration-processing* nil)
(defvar *trace-precondition-processing* nil)
(defvar *trace-unique-name-axiom-processing* nil)

(defvar *assert-rewrite-for-declarations* nil)
(defvar *assert-rewrite-for-preconditions* t)
(defvar *assert-rewrite-for-unique-names-axioms* nil)

(defvar *support-declarations* nil)
(defvar *support-preconditions* nil)
(defvar *support-unique-name-axioms* nil)


(defgeneric set-up-snark (compilation-context)
  (:documentation
   "Set up Snark to prove things in COMPILATION-CONTEXT.")
  (:method ((context compilation-context))
    (iterate (for declaration in-sequence (declarations context))
      (when (and (trace-odysseus-p) *trace-declaration-processing*)
        (format t "~&Processing declaration:~28T~:W" declaration))
      (process-declaration-for-snark
       declaration
       :context context
       :supported *support-declarations*
       :rewrite-too *assert-rewrite-for-declarations*))
    (iterate (for (nil action) in-hashtable (primitive-actions context))
      (let ((precondition (action-precondition action)))
        (when precondition
          (when (and (trace-odysseus-p) *trace-precondition-processing*)
            (format t "~&Processing precondition:~28T~:W"
                    precondition))
          (process-declaration-for-snark
           precondition
           :context context
           :supported *support-preconditions*
           :rewrite-too *assert-rewrite-for-preconditions*))))
    (iterate (for declaration in-sequence (make-unique-names-axioms context))
      (when (and (trace-odysseus-p) *trace-unique-name-axiom-processing*)
        (format t "~&Processing unique names axiom:~28T~:W" declaration))
      (process-declaration-for-snark
       declaration
       :context context
       :supported *support-unique-name-axioms*
       :rewrite-too *assert-rewrite-for-unique-names-axioms*))
    :snark-setup-completed))
