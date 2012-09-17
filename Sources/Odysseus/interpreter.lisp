;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-syntax)

(defclass interpreter-state (compilation-unit)
  ()
  (:documentation
   "The state of the interpreter."))

(defvar *default-interpreter-state*
  (make-instance 'interpreter-state))
(defvar *the-empty-program-term*
  (make-instance 'empty-program-term :context *default-interpreter-state*))

(defgeneric interpret (term &optional state))

(defmethod interpret ((term list)
		      &optional (state *default-interpreter-state*))
  (interpret (parse-into-term-representation term state)))

(defmethod interpret ((term empty-program-term)
		      &optional (state *default-interpreter-state*))
  (declare (ignore state))
  (values :no-action term))

(defmethod interpret ((term primitive-action-term)
		      &optional (state *default-interpreter-state*))
  (declare (ignore state))
  (values (source term)
          *the-empty-program-term*))

(defmethod interpret ((term sequence-term)
		      &optional (state *default-interpreter-state*))
  (let ((body (body term)))
    (cond ((null body)
	   (interpret *the-empty-program-term* state))
	  ((null (rest body))
	   (interpret (first body) state))
	  (t
	   (multiple-value-bind (action rest-term)
	       (interpret (first body) state)
	     (if (is-final-term-p rest-term)
		 (values action
                         (make-instance 'sequence-term
					:context (context term)
					:body (rest body)))
		 (values action
                         (make-instance 'sequence-term
					:context (context term)
					:body (cons rest-term (rest body))))))))))


(defun interpret-and-print (term &optional (state *default-interpreter-state*))
  (multiple-value-bind (action rest-term)
      (interpret term state)
    (print action)
    (if (is-final-term-p rest-term)
        :done
        (interpret-and-print rest-term state))))
