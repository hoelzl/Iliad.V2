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
  (let* ((binding (ensure-list binding))
         (binding-variable
           (parse-into-term-representation (first binding) context))
         (keywords (rest binding)))
    (check-type binding-variable variable-term)
    (setf (is-bound-p binding-variable) t)
    ;; TODO: maybe check that keywords is a plist in the proper form?
    (make-instance 'binding
                   :variable binding-variable
                   :keywords keywords
                   :context context)))

(defmethod parse-arguments-for-term :around ((term binding-term) arguments context)
  "Parse the binding list and then call the next method on the rest of the
argument list."
  (let* ((binding-list (ensure-list (first arguments)))
         (new-context (make-instance 'local-context :outer-context context))
         (bindings (mapcar (lambda (binding)
                             (parse-binding binding term new-context))
                           binding-list)))
    (setf (bindings term) bindings)
    (call-next-method term (rest arguments) new-context)))

(defmethod parse-into-term-representation ((exp symbol) (context compilation-context))
  (cond ((or (eql exp 'nil) (eql exp 'null))
	 (make-instance 'empty-program-term :context context :source exp))
	((starts-with-question-mark-p exp)
	 (make-variable-term exp context))
	(t
	 (make-instance 'primitive-term
                        :value exp :context context :source exp))))

(defmethod parse-into-term-representation ((exp number) (context compilation-context))
  (make-instance 'number-term :value exp :context context))

(defmethod parse-into-term-representation ((exp string) (context compilation-context))
  (make-instance 'primitive-term :value exp :context context))

(defmethod parse-into-term-representation
    ((exp snark::variable) (context compilation-context))
  (parse-into-term-representation
   (intern (format nil "?SV~A.~A"
                   (snark::variable-number exp)
                   (snark::variable-sort exp)))
   context))

(defmethod parse-into-term-representation ((exp cons) (context compilation-context))
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
