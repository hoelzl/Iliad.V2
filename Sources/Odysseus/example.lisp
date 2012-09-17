;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)

(define-primitive-action eat)
(define-primitive-action sleep)
(define-primitive-action celebrate)

(declare-primitive-action 'eat *default-interpreter-state*)
(declare-primitive-action 'sleep *default-interpreter-state*)
(declare-primitive-action 'celebrate *default-interpreter-state*)

(interpret '(seq
	     (eat something)
	     (sleep several hours)
	     (celebrate)))
