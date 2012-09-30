;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-snark)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defvar *print-snark-output* nil)

(defun initialize-snark (&key (run-time-limit 0.1))
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
  (print-options-when-starting nil)
  (print-summary-when-finished nil)
  (print-rows-when-derived nil)
  (print-rows-when-finished nil)
  (print-agenda-when-finished nil)
  (run-time-limit run-time-limit)
  ;; (use-conditional-answer-creation t)
  ;; (use-subsumption-by-false nil)
  ;; (use-constructive-answer-restriction nil)
  (allow-skolem-symbols-in-answers t)
  )


(defun prove-or-refute (term &rest args
			&key context (run-time-limit 0.1)
			&allow-other-keys)
  (assert context (context)
          "Cannot prove or refute without a context.")
  (initialize-snark :run-time-limit run-time-limit)
  (set-up-snark context)
  (let* ((new-args (alexandria:remove-from-plist
                    args
                    :solution-depth :context :run-time-limit))
	 (result (maybe-suppress-snark-output
                   (apply 'snark:prove term new-args))))
    (ecase result
      (:proof-found result)
      ((:run-time-limit :agenda-empty)
       (initialize-snark :run-time-limit run-time-limit)
       (set-up-snark context)
       (let ((result (maybe-suppress-snark-output
                       (apply 'prove `(snark::not ,term) new-args))))
	 (ecase result
	   (:proof-found :refutation-found)
	   (:agenda-empty :agenda-empty)
	   (:run-time-limit :run-time-limit)))))))

(defvar *trace-ida-time-increases* nil)

(defun ida-prove-or-refute (term &rest args &key &allow-other-keys)
  (let* ((initial-run-time-limit 0.05)
	 (run-time-limit initial-run-time-limit)
	 (iterations 4))
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

(defvar *error-when-refutation-without-answer* t)

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

(defun prove-using-snark-closure (term solution-depth answer)
  (when (trace-odysseus-p)
    (format t "~&Trying to prove:~28T~:W" term)
    (format t "~&    Solution depth:~28T~A~%" solution-depth))
  (let ((result (maybe-suppress-snark-output
                  (snark:prove term :answer answer))))
    (ecase result
      (:proof-found
       (compute-closure solution-depth))
      (:agenda-empty
       (values nil :undecidable (snark-answer)))
      (:run-time-limit
       (values nil :timeout nil)))))

(defgeneric prove-using-snark
    (term &rest args &key context answer solution-depth)
  (:documentation
   "Prove TERM using SNARK.")

  (:method ((term odysseus-syntax:term)
            &rest args
            &key context answer (solution-depth 0))
    (declare (ignore context answer solution-depth))
    (apply 'prove-using-snark (odysseus-syntax:to-sexpr term) args))

  (:method ((term cons)
            &rest args
            &key context answer (solution-depth 0))
    (declare (ignore context))
    (unless answer
      (alexandria:remove-from-plistf args :answer))
    (if (= solution-depth 0)
        (prove-using-snark-depth-zero term args)
        (prove-using-snark-closure term solution-depth answer))))
