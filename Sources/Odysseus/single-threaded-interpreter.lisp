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

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term empty-program-term) situation)
  "Return a noop and another empty term (this is probably a bug)."
  (values (the-no-operation-term interpreter)
          term ;; FIXME: This should be an error term.
          situation))

(defvar *store-all-non-refuted-proof-terms* nil
  "If true, add all proof terms (preconditions and tests) that were not
  refuted to the list of deferred proof-terms.")

(defvar *continue-after-undecidable-test* t
  "If true, continue interpretation after undecidable tests without failing.")

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term test-term) situation)
  (multiple-value-bind (holds? reason free-variables answer)
      (prove interpreter (argument term)
             :solution-depth (solution-depth term)
             :quantification-function 'existentially-quantify)
    (maybe-add-choice-point interpreter term situation reason answer)
    (cond (holds?
           (when *store-all-non-refuted-proof-terms*
             (push (argument term) (deferred-proofs interpreter)))
           (multiple-value-setq (term situation)
             (perform-substitutions-in-interpreter
              interpreter term situation free-variables answer))
           (maybe-output-execution-trace-information
            ">>> Successful test:" term reason free-variables answer)
           (values (the-no-operation-term interpreter)
                   (the-empty-program-term interpreter)
                   situation))
          ((or (eql reason :undecidable) (eql reason :timeout))
           (cond (*continue-after-undecidable-test*
                  (push (argument term)
                        (deferred-proofs interpreter))
                  (multiple-value-setq (term situation)
                    (perform-substitutions-in-interpreter
                     interpreter term situation free-variables answer))
                  (maybe-output-execution-trace-information
                   ">>> Continuing after:"
                   term (list reason :continue) free-variables answer)
                  (values (the-no-operation-term interpreter)
                          (the-empty-program-term interpreter)
                          situation))
                 (t
                  (maybe-output-execution-trace-information
                   ">>> Failing after:"
                   term (list reason :fail) free-variables answer)
                  (backtrack interpreter))))
          (t
           (maybe-add-choice-point interpreter term situation reason answer)
           (maybe-output-execution-trace-information
            ">>> Failed test:" term reason free-variables answer)
           (backtrack interpreter)))))

(defvar *continue-after-undecidable-precondition* nil
  "If true, continue interpretation (without failing) after actions with
  undecidable preconditions.")

(defun interpret-1-primitive-action-success
    (interpreter term situation reason free-variables answer)
  "Perform the main work of INTERPRET-1 when TERM should be evaluated, either
because its precondition is true, or because its precondition is undecidable
and we continue anyway."
  (multiple-value-setq (term situation)
    (perform-substitutions-in-interpreter
     interpreter term situation free-variables answer))
  (cond ((onlinep interpreter)
         (maybe-output-execution-trace-information
          ">>> Executing" term reason free-variables answer)
         (values term
                 (the-empty-program-term interpreter)
                 (make-instance 'successor-situation
                   :action term
                   :previous-situation situation)))
        (t
         (maybe-output-execution-trace-information
          "Storing:" term reason free-variables answer)
         (push term (stored-actions interpreter))
         (values (the-no-operation-term interpreter)
                 (the-empty-program-term interpreter)
                 (make-instance 'successor-situation
                   :action term
                   :previous-situation situation)))))


(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term primitive-action-term) situation)
  (multiple-value-bind (can-execute-p reason free-variables answer)
      (can-execute-p interpreter term situation
                     :solution-depth (solution-depth term))
    (maybe-add-choice-point interpreter term situation reason answer)
    (cond (can-execute-p
           (when *store-all-non-refuted-proof-terms*
             (push (precondition-term term situation)
                   (deferred-proofs interpreter)))
           (interpret-1-primitive-action-success
            interpreter term situation reason free-variables answer))
          ((and (or (eql reason :undecidable) (eql reason :timeout))
                *continue-after-undecidable-precondition*)
           (push (precondition-term term situation)
                 (deferred-proofs interpreter))
           (when *trace-odysseus*
             (format t "~&Continuing after undecidable precondition!~%"))
           (interpret-1-primitive-action-success
            interpreter term situation reason free-variables answer))
          (t
           (maybe-output-execution-trace-information
            "NOT Executing:" term reason free-variables answer)
           (backtrack interpreter)))))

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
	  (t
           (push (make-instance (class-of term)
                   :context (context term)
                   :source :generated-term
                   :body (rest body))
                 (stored-continuations interpreter))
	   (multiple-value-bind (action rest-term new-situation)
	       (interpret-1 interpreter (first body) situation)
             (values action
                     (if (is-final-term-p rest-term)
                         (the-empty-program-term interpreter)
                         (make-instance (class-of term)
                           :context (context term)
                           :source :generated-term
                           :body rest-term))
                     new-situation))))))

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term sequence-term) situation)
  (interpret-1-body-term interpreter term situation))

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term search-term) situation)
  (let ((old-onlinep (onlinep interpreter)))
    (setf (onlinep interpreter) nil)
    (multiple-value-bind (action rest-term new-situation)
        (interpret-1-body-term interpreter term situation)
      (setf (onlinep interpreter) old-onlinep)
      (values action
              rest-term
              new-situation))))

(defvar *permute-offline-choice* t)

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term action-choice-term) situation)
  (if (onlinep interpreter)
      (interpret-1 interpreter (random-elt (body term)) situation)
      (progn
        (mapcar (lambda (choice)
                  (add-choice-point interpreter choice situation))
                (if *permute-offline-choice*
                    (shuffle (body term))
                    (body term)))
        (backtrack interpreter :reason "Starting action choice"))))

(defmethod interpret-1
    ((interpreter single-threaded-interpreter) (term declaration-term) situation)
  ;; NOTE: The implementation of INTERPRET-1 for sequence terms picks off
  ;; declarations at the start of sequences.  Therefore, if the implementation
  ;; of this method changes you need to adjust INTERPRET-1-BODY-TERM as well.
  (values (the-no-operation-term interpreter)
          (the-empty-program-term interpreter)
          situation))
