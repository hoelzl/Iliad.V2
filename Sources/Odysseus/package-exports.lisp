;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *odysseus-utilities-exports*
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
      #:odysseus-situation-suite
      #:odysseus-parser-suite
      #:odysseus-interpreter-suite
      #:odysseus-compiler-suite
      #:odysseus-builtins-suite))
  
  (defvar *odysseus-context-exports*
    '(;; Forward declaration
      #:parse-into-term-representation
      
      #:primitive-action-definition #:context
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
      #:primitive-actions #:default-primitive-action-names
      #:fluents
      #:the-empty-program-term #:the-no-operation-term
      #:context-mixin #:context
      #:unique-terms-mixin
      #:compilation-unit
      #:local-context #:outer-context #:local-variables))
  
  (defvar *odysseus-term-exports*
    '(#:term #:source
      #:variable-term #:unique-name #:is-bound-p
      #:make-variable-term 
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
      #:no-operation-term #:no-operation
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
      
      #:to-sexpr
      #:free-variables #:free-variable-sexprs))
  
  (defvar *odysseus-operator-exports*
    '("&" "," ";" "~" "->" "=>" "<-" "<=" "<->" "<=>"
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
      
      #:poss
      
      #:define-primitive-action #:defaction #:defprimitive
      #:primitive-action #:primact
      #:define-procedure #:defprocedure #:defproc
      #:procedure #:proc
      #:define-domain #:defdomain))
  
  (defvar *odysseus-situation-exports*
    '(#:situation
      #:initial-situation #:s0
      #:successor-situation
      #:next-situation
      #:in-situation))
  
  (defvar *odysseus-parser-exports*
    '(#:starts-with-question-mark-p
      #:parse-arguments-for-term
      #:parse-binding
      #:parse-into-term-representation))
  
  (defvar *odysseus-snark-exports*
    '(#:initialize-snark
      #:set-up-theory
      #:*print-snark-output*
      #:prove-using-snark))
  
  (defvar *odysseus-interpreter-exports*
    '(#:interpreter
      #:default-interpreter
      #:reset-interpreter
      #:can-execute-p
      #:make-choice-point #:next-choice-point #:backtrack
      #:state-map #:can-set-state-p #:state
      #:basic-interpreter-state
      #:basic-interpreter

      #:trace-odysseus #:untrace-odysseus

      #:interpret-1
      #:skip-noops #:print-everything
      #:interpret-and-print))
  ) ; eval-when
