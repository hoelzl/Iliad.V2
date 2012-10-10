;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))


;;; Multi-Step Interpretation
;;; =========================

(defvar *instantiate-undecidable-choices* t
  "If true, try to instantiate variables with known constants if a proof
  remains undecidable.")

;;; TODO: This should really be lazy.  Maybe write it as an ITERATE
;;; driver?
(defun instantiations-for (vars context)
  (if (null vars)
      '(())
      (let* ((sort (slot-value (first vars) 'declared-sort))
             (constants-for-sort (constants-for-sort sort context)))
        (mapcan (lambda (value)
                  (mapcar (lambda (inst)
                            (cons value inst))
                          (instantiations-for (rest vars) context)))
                constants-for-sort))))

(defun prove-with-instantiated-variables
    (interpreter term situation vars)
  (iterate (for inst in (instantiations-for vars (context interpreter)))
    (let ((instantiated-term (substitute-terms inst vars term)))
      (multiple-value-bind (result reason free-variables answer)
          (prove interpreter instantiated-term
                 :quantification-function 'existentially-quantify)
        (when result
          (maybe-output-execution-trace-information
           ">>> Successful proof:" 
           instantiated-term reason free-variables answer)
          (multiple-value-bind (new-term new-situation)
              (perform-substitutions-in-interpreter
               interpreter term situation free-variables answer)
            (declare (ignore new-term))
            (execute-stored-actions interpreter)
            (return-from prove-with-instantiated-variables
              (values t new-situation))))))))

(defun maybe-instantiate-variables (interpreter term situation)
  (let ((free-variables (free-variables term))
        (instantiated-variables '()))
    (iterate (for var in free-variables)
      (push var instantiated-variables)
      (multiple-value-bind (result new-situation)
          (prove-with-instantiated-variables
           interpreter term situation instantiated-variables)
        (when result
          (return-from maybe-instantiate-variables
            (values t new-situation))))))
  (values nil :no-situation-available))

;;; FIXME
(defmethod try-to-finish-interpretation ((interpreter basic-interpreter) situation)
  (when *trace-odysseus*
    (format t "~&Starting deferred actions.~%"))
  (let ((deferred-proofs (deferred-proofs interpreter)))
    (if deferred-proofs
        (let ((term (apply 'make-conjunction deferred-proofs)))
          (multiple-value-bind (result reason free-variables answer)
              (prove interpreter
                     term
                     :quantification-function 'existentially-quantify)
            (cond (result
                   (maybe-output-execution-trace-information
                    ">>> Successful proof:" term reason free-variables answer)
                   (multiple-value-bind (new-term new-situation)
                       (perform-substitutions-in-interpreter
                        interpreter term situation free-variables answer)
                     (declare (ignore new-term))
                     (execute-stored-actions interpreter)
                     (values t new-situation)))
                  (t
                   (maybe-instantiate-variables interpreter term situation)))))
        (progn
          (execute-stored-actions interpreter)
          (values t situation)))))

;;; FIXME
(defmethod backtrack ((interpreter interpreter)
                      &key reason (continuation-function 'interpret-1))
  (let ((continuation (next-continuation interpreter)))
    (when (onlinep interpreter)
      (cerror "Backtrack anyway."
              'no-backtracking-in-online-mode))
    (when *trace-odysseus*
      (format t "~&~:[Backtracking~;~:*~A~].~%" reason))
    (setf (interpreter-memento interpreter)
          (interpreter-memento continuation))
    (funcall continuation-function
             interpreter (term continuation) (situation continuation))))


(define-condition no-next-term (online-mode-error)
  ((interpreter :initarg :interpreter))
  (:report (lambda (condition stream)
             (with-slots (interpreter) condition
               (format stream "Interpreter ~A has no next term."
                       interpreter)))))

(define-condition unbound-variable-during-online-execution (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot execute actions containing variables in online mode."))))

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

(defmethod next-continuation ((interpreter basic-interpreter))
  (if (null (continuations interpreter))
      (error 'no-next-continuation)
      ;; This should be controllable by an execution strategy.
      (pop (continuations interpreter))))


(defmethod run-interpreter-loop ((interpreter executing-interpreter) term situation)
  (multiple-value-bind (action new-situation substitution
                        deferred-proofs continuation-generator)
      (interpret-1 interpreter term situation)
    (unless (typep action 'no-operation-term)
      (assert (onlinep interpreter) ()
              "Cannot execute actions while the interpreter is offline."))))


(defvar *suppress-interpretation-errors* t)

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
