;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defglobal *odysseus-utilities-exports*
      '(;; General utilities
        #:defglobal
        #:gethash*
        ;; Three-valued logic
        #:boolean3
        #:and3 #:or3 #:not3
        ;; Macros
        #:define-interning-make-instance
        ;; Support for testing
        #:odysseus-suite
        #:odysseus-utilities-suite
        #:odysseus-macro-suite
        #:odysseus-syntax-suite
        #:odysseus-compiler-suite
        #:odysseus-builtins-suite))

  (defglobal *odysseus-syntax-exports*
      '(#:primitive-action-definition #:context
        #:action-class #:action-precondition
        #:declare-primitive-action #:define-primitive-action
        #:fluent-definition
        #:fluent-class #:fluent-successor-state
        #:relational-fluent-definition
        #:declare-relational-fluent #:define-relational-fluent
        #:functional-fluent-definition
        #:declare-functional-fluent #:define-functional-fluent
        #:name-mixin #:required-name-mixin #:name
        #:arguments-mixin #:arguments
        #:known-term #:is-known-term-p
        
        #:compilation-context
        #:lookup-functor #:lookup-variable #:lookup-number
        #:known-operators #:default-known-operators
        #:compilation-unit
        #:local-context #:outer-context #:local-variables
        
        #:term #:source
        #:variable-term #:unique-name #:make-variable-term 
        #:atomic-term
        #:primitive-term #:value
        #:functor-term
        #:number-term
        #:compound-term #:is-compound-term #:operator
        #:term-type-for-operator
        #:application-term #:arguments
        #:unknown-general-application-term
        #:known-compound-term 
        #:known-application-term
        #:unary-term #:argument
        #:binary-term #:lhs #:rhs
        #:ternary-term #:arg1 #:arg2 #:arg3
        #:known-general-application-term
        #:body-term #:body
        #:binding #:binding-variable #:binding-keywords #:binding-context
        #:binding-term #:bound-variables #:bindings
        
        #:conjunction-term
        #:disjunction-term
        #:negation-term
        #:implication-term
        #:reverse-implication-term
        #:equivalence-term
        #:quantification-term
        #:universal-quantification-term
        #:existential-quantification-term
        
        #:empty-program-term
        #:is-final-term-p
        #:primitive-action-term
        #:test-term
        #:sequence-term
        #:action-choice-term
        #:argument-choice-term
        #:iteration-term
        #:conditional-term
        #:while-loop-term
        #:search-term
        #:concurrent-term
        #:prioritized-concurrent-term
        #:spawn-term
        
        #:definition-term
        #:primitive-action-definition-term
        #:procedure-definition-term
        #:domain-definition-term
        
        ;; Parser
        #:starts-with-question-mark-p
        #:parse-arguments-for-term
        #:parse-into-term-representation
        #:parse-binding
        
        ;; Operators
        "&" "," ";" "~" "->" "=>" "<-" "<=" "<->" "<=>"
        #:and #:or #:not #:implies
        #:implied-by #:is-implied-by
        #:iff #:equiv #:equivalent #:is-equivalent #:are-equivalent
        #:foreach #:each #:forall #:exist #:exists
        
        "?"
        #:test #:holds #:holds?
        #:seq #:sequentially #:begin #:progn
        #:one-of #:choose #:choose-action
        #:pick #:pick-argument #:choose-argument
        #:repeat #:loop #:iterate
        #:if #:while #:search #:offline
        #:concurrently #:in-parallel
        #:prioritized #:when-blocked
        #:spawn #:new-process
        
        #:define-primitive-action #:defaction #:defprimitive
        #:primitive-action #:primact
        #:define-procedure #:defprocedure #:defproc
        #:procedure #:proc
        #:define-domain #:defdomain))

  (defglobal *odysseus-interpreter-exports*
      '(#:interpreter-state #:*default-interpreter-state*
        #:interpret #:interpret-and-print))
) ; eval-when
      
(defpackage #:odysseus-utilities
  (:use #:common-lisp #:alexandria #:iterate)
  (:nicknames #:utils)
  (:export . #.*odysseus-utilities-exports*))

(defpackage #:odysseus-syntax
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities)
  (:nicknames #:syntax)
  (:export . #.*odysseus-syntax-exports*))

(defpackage #:odysseus-interpreter
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities #:odysseus-syntax)
  (:nicknames #:interpreter #:interp)
  (:export . #.*odysseus-interpreter-exports*))

(defpackage #:odysseus
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities
        #:odysseus-syntax
        #:odysseus-interpreter)
  (:export . #.*odysseus-utilities-exports*)
  (:export . #.*odysseus-syntax-exports*)
  (:export . #.*odysseus-interpreter-exports*))

(defpackage #:odysseus-user
  (:use #:common-lisp #:alexandria #:iterate
        #:common-lisp-user
        #:odysseus))

(defpackage #:odysseus-tests
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus))
