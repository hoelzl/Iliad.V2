;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)

(defgeneric to-sexpr (term)
  (:documentation
   "Convert TERM into an S-expression.")

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
           (wrap-in-quote (sentence term))
           (mapcar #'wrap-in-quote (keywords term))))

  (:method ((situation initial-situation))
    (declare (ignore situation))
    'S0)

  (:method ((situation successor-situation))
    `(do ,(to-sexpr (action situation))
         ,(to-sexpr (previous-situation situation)))))


(defmethod print-object ((term variable-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A" (name term))))

(defmethod print-object ((term application-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~:W" (to-sexpr term))))

(defmethod print-object ((self body-term) stream)
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

#+(or)
(defmacro apply* (&rest args)
  `(progn
     (format t "~&Applying Snark function.~%")
     (format t "~&~W~%" (list ,@args))
     (apply ,@args)))

(defmethod process-declaration-for-snark ((declaration sort-declaration-term))
  (apply #'snark:declare-sort (declared-sort declaration) (keywords declaration))
  :declare-sort)

(defmethod process-declaration-for-snark ((declaration subsort-declaration-term))
  (apply #'snark:declare-subsort
         (declared-sort declaration) (supersort declaration) (keywords declaration))
  :declare-subsort)

(defmethod process-declaration-for-snark ((declaration sorts-incompatible-declaration-term))
  (apply #'snark:declare-sorts-incompatible (sorts declaration))
  :declare-sorts-incompatible)

(defmethod process-declaration-for-snark ((declaration constant-declaration-term))
  (apply #'snark:declare-constant (name declaration) (keywords declaration))
  :declare-constant)

(defmethod process-declaration-for-snark ((declaration function-declaration-term))
  (apply #'snark:declare-function (name declaration) (arity declaration) (keywords declaration))
  :declare-function)

(defmethod process-declaration-for-snark ((declaration relation-declaration-term))
  (apply #'snark:declare-relation (name declaration) (arity declaration) (keywords declaration))
  :declare-relation)

(defmethod process-declaration-for-snark ((declaration ordering-declaration-term))
  (apply #'snark:declare-ordering-greaterp (ordered-symbols declaration))
  :declare-ordering-greaterp)

(defmethod process-declaration-for-snark ((declaration logical-assertion-term))
  (apply #'snark::assert (sentence declaration) (keywords declaration))
  :assert)

(defmethod process-declaration-for-snark ((declaration logical-assumption-term))
  (apply #'snark:assume (sentence declaration) (keywords declaration))
  :assume)

(defmethod process-declaration-for-snark ((declaration rewrite-assertion-term))
  (apply #'snark:assert-rewrite (sentence declaration) (keywords declaration))
  :assert-rewrite)

(defmethod process-declaration-for-snark ((declaration primitive-action-declaration-term))
  (let ((signature (signature declaration))
        (keywords (remove-from-plist (keywords declaration) :precondition)))
    (apply #'snark:declare-function
           (name declaration)
           (1- (length signature))
           :sort signature
           keywords))
  :primitive-action/declare-function)

(defmethod process-declaration-for-snark ((declaration relational-fluent-declaration-term))
  (let ((signature (signature declaration)))
    (apply #'snark:declare-relation
           (name declaration)
           (1- (length signature))
           :sort signature
           (keywords declaration)))
  :fluent/declare-relation)

(defmethod process-declaration-for-snark ((declaration functional-fluent-declaration-term))
  (let ((signature (signature declaration)))
    (apply #'snark:declare-function
           (name declaration)
           (1- (length signature))
           :sort signature
           (keywords declaration)))
  :fluent/declare-function)

