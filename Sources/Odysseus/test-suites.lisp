;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Testing
;;; =======

(unless (find-test 'odysseus-suite :otherwise nil)
  (defsuite (odysseus-suite
             :documentation
             "The suite containing all tests for Odysseus.")))

(unless (find-test 'odysseus-utilities-suite :otherwise nil)
  (defsuite (odysseus-utilities-suite
             :documentation "Tests for utilities."
             :in odysseus-suite)))

(unless (find-test 'odysseus-macro-suite :otherwise nil)
  (defsuite (odysseus-macro-suite
             :documentation "Tests for macros."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-syntax-suite :otherwise nil)
  (defsuite (odysseus-syntax-suite
             :documentation "Tests for the syntax representation."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-parser-suite :otherwise nil)
  (defsuite (odysseus-parser-suite
             :documentation "Tests for the parser."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-situation-suite :otherwise nil)
  (defsuite (odysseus-situation-suite
             :documentation "Tests for the situations."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-snark-suite :otherwise nil)
  (defsuite (odysseus-snark-suite
             :documentation "Tests for the Snark interface."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-substitution-suite :otherwise nil)
  (defsuite (odysseus-substitution-suite
             :documentation "Tests for substitutions."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-continuation-suite :otherwise nil)
  (defsuite (odysseus-continuation-suite
             :documentation "Tests for continuations."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-world-suite :otherwise nil)
  (defsuite (odysseus-world-suite
             :documentation "Tests for the worlds abstraction."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-interpreter-suite :otherwise nil)
  (defsuite (odysseus-interpreter-suite
             :documentation "Tests for the interpreter."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-compiler-suite :otherwise nil)
  (defsuite (odysseus-compiler-suite
             :documentation "Tests for the compiler."
             :in odysseus-suite)))
      
(unless (find-test 'odysseus-builtins-suite :otherwise nil)
  (defsuite (odysseus-builtins-suite
             :documentation "Tests for the built-in predicates."
             :in odysseus-suite)))

