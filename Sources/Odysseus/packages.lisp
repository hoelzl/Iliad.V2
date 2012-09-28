;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)
     
(defpackage #:odysseus-utilities
  (:use #:common-lisp #:alexandria #:iterate)
  (:nicknames #:utils)
  (:import-from #:snark-lisp
                #:forall #:exists #:not #:iff)
  (:export . #.*odysseus-utilities-exports*))

(defpackage #:odysseus-syntax
  ;; The odysseus-syntax package contains the implementation of contexts, and
  ;; terms.  We define packages that export subsets of these symbols for use
  ;; by other programs.
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities)
  (:import-from #:snark-lisp
                #:forall #:exists #:not #:iff)
  (:nicknames #:syntax)
  (:export . #.*odysseus-context-exports*)
  (:export . #.*odysseus-term-exports*)
  (:export . #.*odysseus-operator-exports*)
  (:export . #.*odysseus-situation-exports*))

(defpackage #:odysseus-context
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-syntax)
  (:nicknames #:context)
  (:export . #.*odysseus-context-exports*))

(defpackage #:odysseus-terms
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-syntax)
  (:nicknames #:terms)
  (:export . #.*odysseus-term-exports*))

(defpackage #:odysseus-operators
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-syntax)
  (:nicknames #:operators)
  (:export . #.*odysseus-operator-exports*))

(defpackage #:odysseus-situation
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities
        #:odysseus-syntax)
  (:nicknames #:situation)
  (:export . #.*odysseus-situation-exports*))

(defpackage #:odysseus-parser
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities
        #:odysseus-syntax
        #:odysseus-situation)
  (:nicknames #:parser)
  (:export . #.*odysseus-parser-exports*))

(defpackage #:odysseus-snark
  (:use #:common-lisp #:snark
        #:odysseus-utilities #:odysseus-context)
  (:nicknames)
  (:export . #.*odysseus-snark-exports*))

(defpackage #:odysseus-interpreter
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities
        #:odysseus-syntax
        #:odysseus-situation
        #:odysseus-parser
        #:odysseus-snark)
  (:nicknames #:interpreter #:interp)
  (:export . #.*odysseus-interpreter-exports*))

(defpackage #:odysseus
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus-utilities
        #:odysseus-syntax
        #:odysseus-situation
        #:odysseus-parser
        #:odysseus-snark
        #:odysseus-interpreter)
  (:export . #.*odysseus-utilities-exports*)
  (:export . #.*odysseus-context-exports*)
  (:export . #.*odysseus-term-exports*)
  (:export . #.*odysseus-operator-exports*)
  (:export . #.*odysseus-situation-exports*)
  (:export . #.*odysseus-parser-exports*)
  (:export . #.*odysseus-snark-exports*)
  (:export . #.*odysseus-interpreter-exports*))

(defpackage #:odysseus-user
  (:use #:common-lisp #:alexandria #:iterate
        #:common-lisp-user
        #:odysseus))

(defpackage #:odysseus-tests
  (:use #:common-lisp #:alexandria #:iterate
	#:odysseus))
