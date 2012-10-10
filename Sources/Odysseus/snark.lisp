;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-snark)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))
;; (hu.dwim.stefil:in-suite odysseus-snark-suite)

(defvar *print-snark-output* nil)
(defvar *run-time-limit* 0.2)
(defvar *ida-run-time-limit* (/ *run-time-limit* 2))
(defvar *ida-iterations* 3)

(defun initialize-snark (&key (run-time-limit *run-time-limit*))
  (maybe-suppress-snark-output
    (initialize))
  ;; (agenda-length-limit nil)
  (use-hyperresolution t)
  (use-negative-hyperresolution t)
  (use-resolution t)
  (use-ur-resolution t)
  (use-paramodulation t)
  (use-subsumption t)
  (use-constraint-solver-in-subsumption t)
  (use-term-ordering :recursive-path)
  (use-default-ordering t)
  (use-literal-ordering-with-hyperresolution 'literal-ordering-p)
  (use-literal-ordering-with-ur-resolution 'literal-ordering-p)
  (use-literal-ordering-with-paramodulation 'literal-ordering-p)
  (ordering-functions>constants t)
  ;; (print-options-when-starting *print-snark-output*)
  (print-summary-when-finished *print-snark-output*)
  ;; (print-rows-when-derived *print-snark-output*)
  ;; (print-rows-when-finished *print-snark-output*)
  (print-final-rows *print-snark-output*)
  (print-row-answers *print-snark-output*)
  (print-agenda-when-finished *print-snark-output*)
  (run-time-limit run-time-limit)
  ;; (use-indefinite-answers t)
  ;; (use-conditional-answer-creation t)
  (use-subsumption-by-false t)
  (use-constructive-answer-restriction t)
  (allow-skolem-symbols-in-answers nil)
  )

(defun process-snark-args (args answer-vars)
  (setf args (alexandria:remove-from-plist
              args
              :answer-vars :global :solution-depth :context :run-time-limit))
  (if answer-vars
      (list* :answer (cons 'answer answer-vars) args)
      args))

(defun prove-or-refute (term &rest args
			&key answer-vars context (run-time-limit *run-time-limit*)
 			&allow-other-keys)
  (check-type term list)
  (assert context (context)
          "Cannot prove or refute without a context.")
  (initialize-snark :run-time-limit run-time-limit)
  (odysseus:set-up-snark context)
  (let* ((new-args (process-snark-args args answer-vars))
         (result (maybe-suppress-snark-output
                   (apply 'snark:prove term new-args))))
      (ecase result
        (:proof-found result)
        ((:run-time-limit :agenda-empty)
         (initialize-snark :run-time-limit run-time-limit)
         (odysseus:set-up-snark context)
         (let* ((negated-term `(not ,term))
                (result (maybe-suppress-snark-output
                          (apply 'prove negated-term new-args))))
           (ecase result
             (:proof-found :refutation-found)
             (:agenda-empty :agenda-empty)
             (:run-time-limit :run-time-limit)))))))

(defvar *trace-ida-time-increases* nil)

(defun ida-prove-or-refute (term &rest args &key &allow-other-keys)
  (check-type term list)
  (let* ((initial-run-time-limit *ida-run-time-limit*)
	 (run-time-limit initial-run-time-limit)
	 (iterations *ida-iterations*))
    (alexandria:remove-from-plistf args :run-time-limit)
    (dotimes (i iterations)
      (let ((result (apply 'prove-or-refute term
                           :run-time-limit run-time-limit
                           args)))
	(if (eql result :run-time-limit)
	    (progn
              (when (and (trace-odysseus-p) *trace-ida-time-increases*)
                (format t "~&Run time limit reached.  Extending time for proof search.~%"))
              (setf run-time-limit (* 2 run-time-limit)))
	    (return-from ida-prove-or-refute result))))
    (run-time-limit initial-run-time-limit)
    :run-time-limit))

(defvar *error-when-refutation-without-answer* nil)

(define-condition refutation-without-answer (runtime-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "Obtained a refutation without answer.~
                      (This indicates that the action theory is contradictory.)"))))

(defun snark-answer ()
  (let ((result (answer t)))
    (cond ((eql result 'snark-lisp:false)
           (when *error-when-refutation-without-answer*
             (cerror "Return an empty answer."
                     'refutation-without-answer))
           '())
          (t result))))

(defun compute-closure (depth)
  (iterate (repeat depth)
    (let ((result (maybe-suppress-snark-output
                    (snark:closure))))
      (case result
        (:proof-found)
        (:refutation-found
         (leave (values nil :refutation-found (snark-answer))))
        (:agenda-empty
         (leave (values nil :undecidable (snark-answer))))
        (otherwise
         (leave (values nil :timeout nil)))))
    (finally (return (values t :proof-found (snark-answer))))))

(defun prove-using-snark-depth-zero (term args)
  (check-type term list)
  (when (trace-odysseus-p)
    (format t "~&Trying to prove or refute:~28T~:W~%" term)
    (format t "~&    Solution depth:~28T0~%"))
  (let ((result (apply 'ida-prove-or-refute term args)))
    (case result
      (:proof-found
       (values t :proof-found (snark-answer)))
      (:refutation-found
       (values nil :refutation-found (snark-answer)))
      (:agenda-empty
       (values nil :undecidable (snark-answer)))
      (otherwise
       (values nil :timeout nil)))))

(defun prove-using-snark-closure (term solution-depth answer-vars)
  (check-type term list)
  (when (trace-odysseus-p)
    (format t "~&Trying to prove:~28T~:W" term)
    (format t "~&    Solution depth:~28T~A~%" solution-depth))
  (let* ((answer (cons 'answer answer-vars))
         (result (maybe-suppress-snark-output
                  (snark:prove term :answer answer))))
    (ecase result
      (:proof-found
       (compute-closure solution-depth))
      (:agenda-empty
       (values nil :undecidable (snark-answer)))
      (:run-time-limit
       (values nil :timeout nil)))))

(defgeneric prove-using-snark
    (term &rest args &key context answer-vars solution-depth)
  (:documentation
   "Prove TERM using SNARK.")

  (:method ((term odysseus-syntax:term)
            &rest args
            &key context answer-vars (solution-depth 0))
    (declare (ignore context answer-vars solution-depth))
    (apply 'prove-using-snark (to-sexpr term :include-global t) args))

  (:method ((term cons)
            &rest args
            &key context answer-vars (solution-depth 0))
    (declare (ignore context))
    (if (= solution-depth 0)
        (prove-using-snark-depth-zero term args)
        (prove-using-snark-closure term solution-depth answer-vars))))
