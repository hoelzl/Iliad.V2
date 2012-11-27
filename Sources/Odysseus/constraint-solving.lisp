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
#+(or)
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

#+(or)
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

#+(or)
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

#+(or)
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

#+(or)
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

#+(or)
(define-condition no-next-term (online-mode-error)
  ((interpreter :initarg :interpreter))
  (:report (lambda (condition stream)
             (with-slots (interpreter) condition
               (format stream "Interpreter ~A has no next term."
                       interpreter)))))
