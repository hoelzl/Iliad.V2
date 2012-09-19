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
    'null)

  (:method ((situation initial-situation))
    (declare (ignore situation))
    'S0)

  (:method ((situation successor-situation))
    `(do ,(to-sexpr (action situation))
	 ,(to-sexpr (previous-situation situation)))))

(defmethod print-object ((term variable-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~A" (name term))))

(defmethod print-object ((term application-term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (format stream "~:W" (to-sexpr term))))

(defmethod print-object ((self body-term) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~:W" (to-sexpr self))))


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
     (mapcan #'free-variables (body term))))

  (:method ((term successor-situation))
    (union (free-variables (action term))
	   (free-variables (previous-situation term)))))

(defun free-variable-sexprs (term)
  (mapcar 'to-sexpr (free-variables term)))

(defgeneric contains-variable-p (term)
  (:documentation
   "Returns true if TERM contains a variable, false otherwise.")
  
  (:method (term)
    (declare (ignore term))
    nil)

  (:method ((term variable-term))
    (declare (ignore term))
    t)

  (:method ((term application-term))
    (some 'contains-variable-p (arguments term)))

  (:method ((term body-term))
    (some 'contains-variable-p (body term)))

  (:method ((term successor-situation))
    (or (contains-variable-p (action term))
	(contains-variable-p (previous-situation term)))))
    
