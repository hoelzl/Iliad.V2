;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *odysseus-utilities-exports*
    '(;; Errors
      #:runtime-error
      #:invalid-class
      ;; MOP
      #:define-method
      ;; General utilities
      #:unquote
      #:wrap-in-quote #:wrap-in-forall
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
      #:declarations
      #:unique-terms #:add-unique-term
      #:lookup-functor #:lookup-variable #:lookup-number
      #:known-operators #:default-known-operators
      #:primitive-actions #:default-primitive-action-names
      #:fluents
      #:the-empty-program-term #:the-no-operation-term
      #:context-mixin #:context
      #:singleton-terms-mixin
      #:unique-terms-mixin
      #:compilation-unit
      #:local-context #:enclosing-context #:local-variables
      
      #:set-up-snark
      #:process-declaration-for-snark
      #:variables-and-term-for-universal-quantification
      #:make-unique-names-axiom
      #:make-unique-names-axioms))
  
  (defvar *odysseus-term-exports*
    '(#:term #:source
      #:variable-term #:unique-name #:variable-sort #:is-bound-p
      #:make-variable-term 
      #:make-anonymous-variable-term 
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
      #:binding-term #:bound-variables
      
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

      #:arity
      #:declaration-term
      #:keywords-mixin #:keywords
      #:local-context-mixin
      #:unique-term-mixin
      #:declared-sort #:successor-state
      #:named-declaration-term
      #:sort-declaration-term
      #:subsort-declaration-term #:supersort
      #:sorts-incompatible-declaration-term #:sorts
      #:signature-declaration-term #:signature
      #:primitive-action-declaration-term
      #:precondition
      #:functional-fluent-declaration-term
      #:relational-fluent-declaration-term
      #:constant-declaration-term
      #:unique-constant-declaration-term
      #:arity-declaration-term
      #:function-declaration-term
      #:unique-function-declaration-term
      #:relation-declaration-term
      #:ordering-declaration-term #:ordered-symbols

      #:logical-sentence-declaration-term
      #:sentence
      #:logical-assertion-term
      #:logical-assumption-term
      #:rewrite-assertion-term
      
      #:definition-term
      #:primitive-action-declaration-term
      #:procedure-definition-term
      #:domain-definition-term
      
      #:to-sexpr
      #:free-variables #:free-variable-sexprs
      #:contains-variable-p
      #:substitute-term #:substitute-terms))
  
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

      #:declare-sort #:declare-subsort
      #:declare-sorts-incompatible
      #:declare-ordering-greaterp
      #:declare-constant
      #:declare-unique-constant #:declare-unique-name
      #:declare-function
      #:declare-unique-function
      #:declare-relation
      #:declare-unique-relation
      #:assert #:assert-rewrite
      #:assume
      
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
      #:process-declaration-for-parsing
      #:parse-arguments-for-term
      #:parse-binding
      #:destructure-variable-name
      #:parse-variable-term
      #:parse-into-term-representation))
  
  (defvar *odysseus-snark-exports*
    '(#:initialize-snark
      #:set-up-theory
      #:*print-snark-output*
      #:prove-using-snark))
  
  (defvar *odysseus-interpreter-exports*
    '(#:no-state-for-situation
      #:no-next-choice-point
      #:online-mode-error
      #:no-backtracking-in-online-mode
      #:no-choice-point-creation-in-online-mode
      #:unbound-variable-during-online-execution

      #:interpreter
      #:reset-interpreter
      #:interpreter-memento
      #:prove
      #:can-execute-p
      #:choice-point
      #:term #:situation #:interpreter-memento
      #:make-choice-point #:next-choice-point #:backtrack
      #:stored-actions
      #:execute-stored-actions #:execute-primitive-action
      #:state-map #:can-set-state-p #:state
      #:maybe-output-trace-information
      #:perform-substitutions-in-interpreter

      ;; TODO: should belong to the configuration framework.
      #:*permute-offline-choice*

      #:primitive-action-definition
      ;; These are originally exported from the context exports.  Not sure if
      ;; we should have them here as well. --tc
      #:the-empty-program-term #:the-no-operation-term
      #:declare-primitive-action
      #:declare-relational-fluent #:declare-functional-fluent

      #:trace-odysseus #:untrace-odysseus

      #:top-level-context
      #:basic-interpreter
      #:choice-points #:onlinep
      #:printing-interpreter

      #:default-interpreter

      #:interpret-1
      #:skip-noops #:print-everything
      #:interpret

      #:answer))
  ) ; eval-when

