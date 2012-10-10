;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; The Class SINGLE-THREADED-INTERPRETER
;;; =====================================

;;; A single-threaded interpreter implements all operations that can
;;; be performed by a single thread, and calls its superordinate
;;; interpreter for operations that deal with threads.  This allows us
;;; to separate the semantics of the core language from the details of
;;; concurrent behavior and simplifies the implementation of various
;;; concurrent interpreters with different concurrency models.

(defclass single-threaded-interpreter (basic-interpreter)
  ()
  (:documentation
   "An interpreter that executes a single thread."))


;;; Single-Step Interpretation of single threads
;;; --------------------------------------------

(defun backtrack
    (&optional (continuation-generator (the-empty-continuation-generator)))
  (values nil nil (the-empty-substitution) nil continuation-generator))

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term empty-program-term) situation)
  "Return a NO-OPERATION-TERM`, the current situation, an empty situation, NIL
to indicate no new proof oblications and an empty continuation generator."
  (values (the-no-operation-term interpreter)
          situation
          (the-empty-substitution)
          nil
          (the-empty-continuation-generator)))


(defmethod make-continuation-generator
    ((interpreter single-threaded-interpreter) term situation deferred-proofs
     reason answer)
    "The default method for SINGLE-THREADED-INTERPRETER returns a continuation
generator only if a proof was found that contains substitutions that do not
only consist of variables."
    (if (proof-and-substitution-found reason answer)
        (make-instance 'continuation-generator
          :interpreter interpreter
          :term (clone-multi-solution-term-increasing-depth term)
          :situation situation
          :deferred-proofs deferred-proofs)
        (the-empty-continuation-generator)))

(defmethod make-continuation-generator
    ((interpreter single-threaded-interpreter) (term primitive-action-term)
     situation deferred-proofs reason answer)
    "Never generate new continuations for primitive actions without
preconditions since there is no way that they can produce variable bindings."
  (declare (ignore situation deferred-proofs reason answer))
  (if (action-precondition term)
      (call-next-method)
      (the-empty-continuation-generator)))


(defvar *continue-after-undecidable-test* t
  "If true, continue interpretation after undecidable tests without failing.")

(defun interpret-1-test-success
    (interpreter term situation
     &key (reason (required-argument :reason))
          (free-variables (required-argument :free-variables))
          (answer (required-argument :answer))
          (deferred-proofs nil)
          (log-message (required-argument :log-message)))
  "Perform the main work of INTERPRET-1 when TERM should be evaluated, either
because its test evaluated to true, or because its precondition is undecidable
and we continue anyway."
  (declare (type interpreter interpreter)
           (type test-term term)
           (type situation situation)
           (type list free-variables)
           (type list answer)
           (type (or null term) deferred-proofs)
           (type string log-message))
  (let* ((continuation-generator
           (make-continuation-generator
            interpreter term situation '() reason answer))
         (substitution (make-substitution-for-interpreter
                        interpreter free-variables answer)))
    ;; We only need the substitution in TERM for tracing.  The
    ;; substitution operation could be optimized away when no trace
    ;; output is generated.
    (setf term (apply-substitution term substitution)
          situation (apply-substitution situation substitution))
    (maybe-output-execution-trace-information
     log-message term reason free-variables answer)
    (values (the-no-operation-term interpreter)
            situation
            substitution
            deferred-proofs
            continuation-generator)))

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term test-term) situation)
  (multiple-value-bind (holds? reason free-variables answer)
      (prove interpreter (argument term)
             :solution-depth (solution-depth term)
             :quantification-function 'existentially-quantify)
      (cond (holds?
             (interpret-1-test-success interpreter term situation
                                       :reason reason
                                       :free-variables free-variables
                                       :answer answer
                                       :log-message ">>> Successful test:"))
            ((or (eql reason :undecidable) (eql reason :timeout))
             (cond (*continue-after-undecidable-test*
                    (interpret-1-test-success interpreter term situation
                                              :reason (list reason :continue)
                                              :free-variables free-variables
                                              :answer answer
                                              :deferred-proofs (argument term)
                                              :log-message ">>> Continuing after:"))
                   (t
                    (maybe-output-execution-trace-information
                     ">>> Failing after:" term (list reason :fail)
                     free-variables answer)
                    (backtrack))))
            (t
             (maybe-output-execution-trace-information
              ">>> Failed test:" term reason free-variables answer)
             (backtrack)))))


(defvar *continue-after-undecidable-precondition* nil
  "If true, continue interpretation (without failing) after actions with
  undecidable preconditions.")

(defun interpret-1-primitive-action-success
    (interpreter term situation
     &key (reason (required-argument :reason))
          (free-variables (required-argument :free-variables))
          (answer (required-argument :answer))
          (deferred-proofs nil))
  "Perform the main work of INTERPRET-1 when TERM should be evaluated, either
because its precondition is true, or because its precondition is undecidable
and we continue anyway."
  (declare (type primitive-action-term term)
           (type situation situation)
           (type list free-variables)
           (type list answer)
           (type (or null term) deferred-proofs))
  (let* ((continuation-generator
           (make-continuation-generator
            interpreter term situation '() reason answer))
         (substitution (make-substitution-for-interpreter
                        interpreter free-variables answer)))
    (setf term (apply-substitution term substitution)
          situation (apply-substitution situation substitution))
    (maybe-output-execution-trace-information
     ">>> Successful precondition:" term reason free-variables answer)
    (values term
            (make-instance 'successor-situation
              :action term
              :previous-situation situation)
            substitution
            deferred-proofs
            continuation-generator)))


(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term primitive-action-term) situation)
  (multiple-value-bind (can-execute-p reason free-variables answer)
      (can-execute-p interpreter term situation
                     :solution-depth (solution-depth term))
    (cond (can-execute-p
           (interpret-1-primitive-action-success interpreter term situation
                                                 :reason reason
                                                 :free-variables free-variables
                                                 :answer answer))
          ((and (or (eql reason :undecidable) (eql reason :timeout))
                *continue-after-undecidable-precondition*)
           (when *trace-odysseus*
             (format t "~&Continuing after undecidable precondition!~%"))
           (interpret-1-primitive-action-success
            interpreter term situation
            :reason reason
            :free-variables free-variables
            :answer answer
            :deferred-proofs (precondition-term term situation)))
          (t
           (maybe-output-execution-trace-information
            "NOT Executing:" term reason free-variables answer)
           (backtrack)))))

(defvar *optimize-interpretation-of-declarations* t)

(defun interpret-1-body-term (interpreter term situation)
  (let ((body (body term)))
    ;; Since most programs start with a long list of declarations that do
    ;; nothing, pick them off here.  This makes the traces of INTERPRET-1 more
    ;; readable.  (It is also marginally faster, but the difference is
    ;; negligible).
    (when *optimize-interpretation-of-declarations*
      (iterate (repeat (length body))
        (if (typep (first body) 'declaration-term)
            (setf body (rest body))
            (leave))))
    (cond ((null body)
	   (interpret-1 interpreter
                        (the-empty-program-term interpreter)
                        situation))
	  ((multiple-value-bind (action new-situation substitution
                                 deferred-proofs continuation-generator)
               (interpret-1 interpreter (first body) situation)
             (values action
                     new-situation
                     substitution
                     deferred-proofs
                     (extend-continuations
                      continuation-generator (rest body) substitution)))))))

;;; In the new semantics the interpretation of SEQ and SEARCH is
;;; exactly the same, since the one-step interpreter does not care (or
;;; even know) about the execution mode.  The difference is only
;;; visible in the driver.

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term sequence-term) situation)
  (interpret-1-body-term interpreter term situation))

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term search-term) situation)
  (interpret-1-body-term interpreter term situation))

(defvar *permute-offline-choice* t)

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term action-choice-term) situation)
  (let ((continuations
          (mapcar (lambda (choice)
                    (make-continuation interpreter choice situation nil))
                  (if *permute-offline-choice*
                      (shuffle (body term))
                      (body term)))))
    (values nil nil (the-empty-substitution) nil continuations)))

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term declaration-term) situation)
  ;; NOTE: The implementation of INTERPRET-1 for sequence terms picks off
  ;; declarations at the start of sequences.  Therefore, if the implementation
  ;; of this method changes you need to adjust INTERPRET-1-BODY-TERM as well.
  ;; (This will definitely happen once we have functions and macros, since we
  ;; can then remove the special treatment of declarations in the parser.)
  (values (the-no-operation-term interpreter)
          situation
          (the-empty-substitution)
          nil
          (the-empty-continuation-generator)))
