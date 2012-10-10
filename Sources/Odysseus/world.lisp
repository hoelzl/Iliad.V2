;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defclass world ()
  ((situation
    :accessor situation :initarg :situation
    :initform (make-instance 'initial-situation)
    :documentation
    "The situation in which the world is right now for inference
    purposes.  This state should always be equal to the situation
    resulting from executing STORED-ACTIONS in INITIAL-SITUATION.")
   (initial-situation
    :accessor initial-situation :initarg :initial-situation
    :documentation
    "The initial situation in which the world was created.")
   (stored-actions
    :accessor stored-actions :initarg :stored-actions
    :initform '()
    :documentation
    "The actions that still have to be executed until WORLD will
    actually be in its SITUATION.")))

(define-condition world-without-initial-situation (runtime-error)
  ((world :initarg :world))
  (:report (lambda (condition stream)
             (with-slots (world) condition
               (format stream "World ~A has no initial situation." world)))))

(defmethod initialize-instance :after
    ((self world) &key (initial-situation nil initial-situation-provided-p))
  (declare (ignore initial-situation))
  ;; Don't complain if somebody has already set the initial situation or if an
  ;; explicit value of NIL is provided.
  (unless (or (slot-boundp self 'initial-situation)
              initial-situation-provided-p)
    (if-let (situation (situation self))
      (setf (initial-situation self) situation)
      (cerror "Continue without setting the initial situation."
              'world-without-initial-situation))))

(defmethod print-object ((self world) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~:W" (to-sexpr (situation self)))))
