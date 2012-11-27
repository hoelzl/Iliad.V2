;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))


(define-condition unbound-variable-during-online-execution (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot execute actions containing variables in online mode."))))

#+(or)
(define-condition no-continuation-creation-in-online-mode (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot create a choice point in online mode."))))

(define-condition no-backtracking-in-online-mode (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "Cannot backtrack to a previous choice point in online mode."))))

(define-condition no-next-continuation (runtime-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Tried to backtrack, but no continuation is left.")))) 


(defclass standard-executing-interpreter (executing-interpreter)
  ((continuation-generator
    :accessor continuation-generator
    :initform (make-instance 'continuation-generator)
    :initarg :continuation-generator))
  (:documentation
   "The basic executing interpreter that implements the standard semantics of
   Poem."))

(defmethod next-continuation ((interpreter standard-executing-interpreter))
  (let* ((cgen (continuation-generator interpreter))
         (cont (pop-next-continuation cgen)))
    (if (not cont)
        (error 'no-next-continuation)
        cont)))

#+(or)
(defmethod run-interpreter-loop ((interpreter executing-interpreter) term situation)
  (multiple-value-bind (action new-situation substitution
                        deferred-proofs continuation-generator)
      (interpret-1 interpreter term situation)
    (unless (typep action 'no-operation-term)
      (assert (onlinep interpreter) ()
              "Cannot execute actions while the interpreter is offline."))))


(defvar *suppress-interpretation-errors* t)

#+(or)
(defun interpret (term &key (interpreter (default-interpreter))
                            (situation (make-instance 'initial-situation))
                            (error-value nil))
  (labels ((recurse (interpreter term situation)
             (multiple-value-bind (action rest-term new-situation)
                 (interpret-1 interpreter term situation)
               (unless (typep action 'no-operation-term)
                 (assert (onlinep interpreter) ()
                         "Cannot execute actions while the interpreter is offline.")
                 (execute-stored-actions interpreter))
               (execute-primitive-action interpreter action)
               (if (is-final-term-p rest-term)
                   (if (can-continue-execution-p interpreter)
                       (recurse interpreter
                                (next-term interpreter)
                                new-situation)
                       (multiple-value-bind (successp new-situation)
                           (try-to-finish-interpretation interpreter new-situation)
                         (if successp
                             (values t (to-sexpr new-situation))
                             (backtrack interpreter
                                        :reason "Deferred actions failed"
                                        :continuation-function #'recurse))))
                   (recurse interpreter rest-term new-situation)))))
    (if *suppress-interpretation-errors*
        (handler-case
            (recurse interpreter term situation)
          (runtime-error (condition)
            (values nil
                    (or error-value (class-name (class-of condition))))))
        (recurse interpreter term situation))))

;;; Default Interpreter
;;; ===================

(defvar *default-interpreter*
  (make-instance 'printing-interpreter))

(defun default-interpreter ()
  "Returns the default interpreter."
  *default-interpreter*)

(defun (setf default-interpreter) (new-interpreter)
  "Sets the default interpreter."
  (setf *default-interpreter* new-interpreter))

(defun default-context ()
  (context (default-interpreter)))
