;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))
(in-suite odysseus-situation-suite)

(defclass situation ()
  ()
  (:documentation
   "The superclass of all situations."))

(defclass initial-situation (situation)
  ()
  (:documentation
   "The initial situation, i.e., the root of a tree of situations."))

(defclass successor-situation (situation)
  ((action :accessor action :initarg :action
	   :initform (required-argument :action))
   (previous-situation :accessor previous-situation
		       :initarg :previous-situation
		       :initform (required-argument :previous-situation)
		       :type situation)))

(defgeneric in-situation (term situation)
  (:documentation
   "Replace the initial situation with SITUATION in TERM."))
