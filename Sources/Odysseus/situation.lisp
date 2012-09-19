;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-situation)

(defclass situation ()
  ()
  (:documentation
   "The superclass of all situations."))

(defclass initial-situation (situation)
  ()
  (:documentation
   "The initial situation, i.e., the root of a tree of situations."))

(defmethod to-sexpr ((situation initial-situation))
  (declare (ignore situation))
  'S0)

(defclass successor-situation (situation)
  ((action :accessor action :initarg :action
	   :initform (required-argument :action))
   (previous-situation :accessor previous-situation
		       :initarg :previous-situation
		       :initform (required-argument :previous-situation)
		       :type situation)))

(defmethod to-sexpr ((situation successor-situation))
  `(do ,(to-sexpr (action situation))
       ,(to-sexpr (previous-situation situation))))

(defgeneric in-situation (term situation)
  (:documentation
   "Replace the initial situation with SITUATION in TERM."))
	
