;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defclass substitution ()
  ()
  (:documentation
   "A substitution that can be applied to terms, continuations, etc."))

(defgeneric apply-substitution (thing substitution)
  (:documentation
   "Apply SUBSTITUTION to THING.")

  (:argument-precedence-order substitution thing)

  (:method ((self null) (subst substitution))
    nil)

  (:method ((self cons) (subst substitution))
    (mapcar (rcurry 'apply-substitution) self))

  (:method ((self initial-situation) (subst substitution))
    (substitute-terms (new-terms subst) (old-terms subst) self))
	
  (:method ((self successor-situation) (subst substitution))
    (make-instance (class-of self)
      :action (apply-substitution (action self) subst)
      :previous-situation (apply-substitution (previous-situation self) subst))))


(defclass empty-substitution (substitution)
  ()
  (:documentation
   "The empty substitution."))

(defmethod apply-substitution (thing (substitution empty-substitution))
  (declare (ignore substitution))
  thing)

(defvar *the-empty-substitution* (make-instance 'empty-substitution))

(defun the-empty-substitution ()
  *the-empty-substitution*)

(defclass non-empty-substitution (substitution)
  ((old-terms :accessor old-terms :initarg :old-terms
	      :initform (required-argument :old-terms))
   (new-terms :accessor new-terms :initarg :new-terms
	      :initform (required-argument :new-terms))))

(defmethod apply-substitution ((term term) (subst non-empty-substitution))
  (substitute-terms (new-terms subst) (old-terms subst) term))

(define-condition invalid-number-of-substitution-terms (runtime-error)
  ((old-terms :initarg :old-terms)
   (new-terms :initarg :new-terms))
  (:report (lambda (condition stream)
             (with-slots (old-terms new-terms) condition
               (format stream
                      "Number of old terms (~A) and new terms (~A) differ."
                      (length old-terms)
                      (length new-terms))))))

(defmethod make-instance ((class (eql (find-class 'substitution)))
                          &key old-terms new-terms)
  (let ((old-length (length old-terms))
        (new-length (length new-terms)))
    (when (not (= old-length new-length))
      (cerror "Create the substitution anyway."
              'invalid-number-of-substitution-terms
              :old-terms old-terms :new-terms new-terms))
    (if (zerop old-length)
        *the-empty-substitution*
        (make-instance 'non-empty-substitution
          :old-terms old-terms :new-terms new-terms))))
