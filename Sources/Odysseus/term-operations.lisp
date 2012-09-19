;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)

(defgeneric to-sexpr (term)
  (:documentation
   "Convert TERM into an S-expression.")

  (:method ((term variable-term))
    (name term))

  (:method ((term number-term))
    (value term))

  (:method ((term primitive-term))
    (value term))
  
  (:method ((term functor-term))
    (symbolicate (name term) "/" (arity term)))

  (:method ((term application-term))
    (list* (operator term)
	   (mapcar 'to-sexpr (arguments term))))

  (:method ((term body-term))
    (list* (operator term)
	   (mapcar 'to-sexpr (body term))))

  (:method ((term quantification-term))
    (list (operator term)
	  (mapcar 'to-sexpr (bound-variables term))
	  (to-sexpr (argument term))))

  (:method ((term empty-program-term))
    'null))

(defgeneric free-variables (term)
  (:documentation
   "Returns a list containing the free variables of TERM.")

  (:method (term)
    (declare (ignore term))
    '())

  (:method ((term variable-term))
    (if (is-bound-p term)
	'()
	(list term)))

  (:method ((term application-term))
    (remove-duplicates
     (mapcan #'free-variables (arguments term))))

  (:method ((term body-term))
    (remove-duplicates
     (mapcan #'free-variables (body term)))))

(defun free-variable-sexprs (term)
  (mapcar 'to-sexpr (free-variables term)))
