;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; The Class CONTINUATION
;;; ======================

(defgeneric term (continuation)
  (:documentation
   "Return the term that represents the future computation of CONTINUATION."))

(defgeneric situation (continuation)
  (:documentation
   "Returns the situation in which CONTINUATION can be applied, i.e., the
   situation before executing TERM of CONTINUATION."))

(defgeneric deferred-proofs (continuation)
  (:documentation
   "Returns the proof obligations that have to be discharged before the TERM
   of CONTINUATION may be applied to its SITUATION."))

(defclass continuation ()
  ((term
    :accessor term :initarg :term
    :initform (required-argument :term)
    :documentation
    "The term that should be used to resume the execution.")
   (situation
    :accessor situation :initarg :situation
    :initform (required-argument :situation)
    :documentation "The situation in which the choice point was captured.")
   (deferred-proofs
    :accessor deferred-proofs :initarg :deferred-proofs
    :initform '()))
  (:documentation
   "A CONTINUATION stores all information required to continue a computation."))

(defmethod print-object ((self continuation) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~:W" (to-sexpr (term self)))))

#+(or)
(defclass repeated-continuation (continuation)
  ((max-repetitions :accessor max-repetitions :initarg :max-repetitions
                    :initform (required-argument :max-repetitions))
   (current-repetition :accessor current-repetition :initarg :current-repetition
                       :initform 0)))

(defmethod apply-substitution ((cont continuation)
			       (subst non-empty-substitution))
  (make-instance (class-of cont)
    :term (apply-substitution (term cont) subst)
    :situation (apply-substitution (situation cont) subst)
    :deferred-proofs (apply-substitution (deferred-proofs cont) subst)))


;;; The Class CONTINUATION-GENERATOR
;;; ================================

(defclass continuation-generator ()
  ()
  (:documentation
   "A CONTINUATION-GENERATOR represents a lazy sequence of continuations."))

(defgeneric continuations (continuation-generator)
  (:documentation
   "Returns the list of all continuations that CONTINUATION-GENERATOR
   can provide without changing the internal state of
   CONTINUATION-GENERATOR."))

(defgeneric peek-next-continuation (continuation-generator)
  (:documentation
   "Returns the next continuation of CONTINUATION-GENERATOR and returns it
   without changing the internal state of CONTINUATION-GENERATOR.  Returns NIL
   if no next continuation exists."))

(defgeneric pop-next-continuation (continuation-generator)
  (:documentation
   "Returns the next continuation of CONTINUATION-GENERATOR and removes it
   from the sequence of continuations that CONTINUATION-GENERATOR will
   produce.  Returns NIL if no next continuation exists."))

(defgeneric append-continuations (gen1 gen2)
  (:documentation
   "Returns a CONTINUATION-GENERATOR that returns the continuations of
   generators gen1 and gen2.  May be identical to one of its arguments.")
  (:method ((gen1 continuation-generator) (gen2 continuation-generator))
    (make-instance 'continuation-generator
      :continuations (append (continuations gen1) (continuations gen2)))))

(defmethod print-object ((self continuation-generator) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~:W" (continuations self))))

(defmethod apply-substitution ((gen continuation-generator)
                               (subst non-empty-substitution))
  (make-instance (class-of gen)
    :continuations (mapcar (rcurry #'apply-substitution subst)
                           (continuations gen))))



;;; The Class EMPTY-CONTINUATION-GENERATOR
;;; --------------------------------------

(defclass empty-continuation-generator (continuation-generator)
  ()
  (:documentation
   "A CONTINUATION-GENERATOR that can never contain any elements."))

(defmethod continuations ((gen empty-continuation-generator))
  (declare (ignore gen))
  '())

(defmethod peek-next-continuation ((gen empty-continuation-generator))
  (declare (ignore gen))
  nil)

(defmethod pop-next-continuation ((gen empty-continuation-generator))
  (declare (ignore gen))
  nil)

(defmethod append-continuations ((gen1 continuation-generator)
                                 (gen2 empty-continuation-generator))
  (declare (ignore gen2))
  gen1)

(defmethod append-continuations ((gen1 empty-continuation-generator)
                                 (gen2 continuation-generator))
  (declare (ignore gen1))
  gen2)

(defvar *the-empty-continuation-generator*
  (make-instance 'empty-continuation-generator))

(defun the-empty-continuation-generator ()
  *the-empty-continuation-generator*)


;;; The Class LIST-CONTINUATION-GENERATOR
;;; -------------------------------------

(defclass list-continuation-generator (continuation-generator)
  ((continuations :accessor continuations
                  :initform '()))
  (:documentation
   "A continuation generator that explicitly stores its continuations in a
   list."))

(defmethod initialize-instance :after
  ((self list-continuation-generator) &key continuations)
  (setf (continuations self)
        (coerce continuations 'list)))

(defmethod peek-next-continuation ((gen list-continuation-generator))
  (first (continuations gen)))

(defmethod pop-next-continuation ((gen list-continuation-generator))
  (pop (continuations gen)))


(defmethod make-instance ((class (eql (find-class 'continuation-generator)))
			  &rest args &key continuations)
  (if continuations
      (apply #'make-instance 'list-continuation-generator args)
      *the-empty-continuation-generator*))



(defgeneric extend-continuation (continuation terms substitution)
  (:documentation
   "Extends CONTINUATION so that it executes its original term followed by
   TERMS, and applies SUBSTITUTION to the new continuation.  May destructively
   modify CONTINUATION or may return a new CONTINUATION.")

  ;; Maybe we should flatten the new body term if both the term of the
  ;; continuation is already a body term?
  (:method (continuation (term term) substitution)
    (setf (term continuation)
          (make-instance 'body-term
            :body (list (apply-substitution (term continuation) substitution)
                        (apply-substitution term substitution))
            :context (context (term continuation))
            :source :generated-term))
    continuation)

  (:method (continuation (term term) (substitution empty-substitution))
    (setf (term continuation)
          (make-instance 'body-term
            :body (list (term continuation) term)
            :context (context (term continuation))
            :source :generated-term))
    continuation)
  
  (:method (continuation (terms null) substitution)
    (setf (term continuation)
          (apply-substitution (term continuation) substitution))
    continuation)
  
  (:method (continuation (terms null) (substitution empty-substitution))
    continuation)
  
  (:method (continuation (terms cons) substitution)
    (setf (term continuation)
          (make-instance 'body-term
            :body (list* (apply-substitution (term continuation) substitution)
                         (mapcar (rcurry #'apply-substitution substitution)
                                 terms))
            :context (context (term continuation))
            :source :generated-term))
    continuation)
  
  (:method (continuation (terms cons) (substitution empty-substitution))
    (setf (term continuation)
          (make-instance 'body-term
            :body (list* (term continuation) terms)
            :context (context (term continuation))
            :source :generated-term))
    continuation))

(defgeneric extend-continuations (continuations terms substitution)
  (:documentation
   "Extend each continuation in CONTINUATIONS so that it executes its
   original actions followed by TERMS.")
  (:method (continuation (term term) substitution)
    (extend-continuations continuation (list term) substitution))
  (:method ((continuations sequence) (terms list) substitution)
    (map (class-of continuations)
         (rcurry 'extend-continuation terms substitution) continuations))
  (:method ((gen continuation-generator) (terms list) substitution)
    (make-instance 'continuation-generator
      :continuations (extend-continuations (continuations gen)
                                           terms
                                           substitution))))
