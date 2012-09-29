;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)

(defpackage #:odysseus
  (:use #:common-lisp #:alexandria #:iterate)
  (:import-from #:snark-lisp
                #:forall #:exists #:not #:iff)
  (:export . #.*odysseus-utilities-exports*)
  (:export . #.*odysseus-context-exports*)
  (:export . #.*odysseus-term-exports*)
  (:export . #.*odysseus-operator-exports*)
  (:export . #.*odysseus-situation-exports*)
  (:export . #.*odysseus-parser-exports*)
  (:export . #.*odysseus-snark-exports*)
  (:export . #.*odysseus-interpreter-exports*))
     
(defpackage #:odysseus-utilities
  (:use #:odysseus)
  (:nicknames #:utils)
  (:export . #.*odysseus-utilities-exports*))

(defpackage #:odysseus-syntax
  ;; The odysseus-syntax package contains the implementation of contexts, and
  ;; terms.  We define packages that export subsets of these symbols for use
  ;; by other programs.
  (:use #:odysseus)
  (:nicknames #:syntax)
  (:export . #.*odysseus-context-exports*)
  (:export . #.*odysseus-term-exports*)
  (:export . #.*odysseus-operator-exports*)
  (:export . #.*odysseus-situation-exports*))

(defpackage #:odysseus-context
  (:use #:odysseus)
  (:nicknames #:context)
  (:export . #.*odysseus-context-exports*))

(defpackage #:odysseus-terms
  (:use #:odysseus)
  (:nicknames #:terms)
  (:export . #.*odysseus-term-exports*))

(defpackage #:odysseus-operators
  (:use #:odysseus)
  (:nicknames #:operators)
  (:export . #.*odysseus-operator-exports*))

(defpackage #:odysseus-situation
  (:use #:odysseus)
  (:nicknames #:situation)
  (:export . #.*odysseus-situation-exports*))

(defpackage #:odysseus-parser
  (:use #:odysseus)
  (:nicknames #:parser)
  (:export . #.*odysseus-parser-exports*))

(defpackage #:odysseus-snark
  (:use #:common-lisp #:snark #:iterate
        #:odysseus-utilities #:odysseus-context)
  (:nicknames #:osnark)
  (:import-from #:odysseus
                . #.*odysseus-snark-exports*)
  (:export . #.*odysseus-snark-exports*))

(defpackage #:odysseus-interpreter
  (:use #:odysseus)
  (:nicknames #:interpreter #:interp)
  (:export . #.*odysseus-interpreter-exports*))

(defpackage #:odysseus-user
  (:use #:common-lisp #:alexandria #:iterate
        #:common-lisp-user
        #:odysseus))

(defpackage #:odysseus-tests
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus))
