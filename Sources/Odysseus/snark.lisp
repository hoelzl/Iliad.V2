;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-snark)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defun initialize-snark ()
  (initialize)
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
  (print-rows-when-finished t)
  (print-agenda-when-finished t)
  (run-time-limit 0.1)
  (use-conditional-answer-creation t)
  ;; (use-subsumption-by-false nil)
  (use-constructive-answer-restriction nil)
  (allow-skolem-symbols-in-answers nil))


(defun prove-or-refute (term &rest args
			&key context
			&allow-other-keys)
  (assert context (context)
          "Cannot prove or refute without a context.")
  (initialize-snark)
  (set-up-snark context)
  (let* ((new-args (alexandria:remove-from-plist args :set-up-theory :context))
	 (result (apply 'prove term new-args)))
    (ecase result
      (:proof-found result)
      ((:run-time-limit :agenda-empty)
       (initialize-snark)
       (set-up-snark context)
       (let ((result (apply 'prove `(snark::not ,term) new-args)))
	 (ecase result
	   (:proof-found :refutation-found)
	   (:agenda-empty :agenda-empty)
	   (:run-time-limit result)))))))

(defun ida-prove-or-refute (term &rest args &key context &allow-other-keys)
  (let* ((initial-run-time-limit 0.1)
	 (run-time-limit initial-run-time-limit)
	 (iterations 20))
    (dotimes (i iterations)
      (initialize-snark)
      (run-time-limit run-time-limit)
      (set-up-snark context)
      (let ((result (apply 'prove-or-refute term args)))
	(if (eql result :run-time-limit)
	    (setf run-time-limit (* 2 run-time-limit))
	    (return-from ida-prove-or-refute result))))
    (run-time-limit initial-run-time-limit)
    :run-time-limit))

(defvar *print-snark-output* nil)

(defgeneric prove-using-snark (term &rest args &key context answer)
  (:documentation
   "Prove TERM using SNARK.")
  (:method ((term odysseus-syntax:term) &rest args &key &allow-other-keys)
    (apply 'prove-using-snark (odysseus-syntax:to-sexpr term) args))
  (:method ((term cons) &rest args &key context answer)
    (declare (ignore context))
    (unless answer
      (alexandria:remove-from-plistf args :answer))
    (flet ((do-prove ()
             (let ((result (apply 'ida-prove-or-refute term args)))
               (case result
                 (:proof-found
                  (values t :proof-found (answer t)))
                 (:refutation-found
                  (values nil :refutation-found (answer t)))
                 (otherwise
                  (values nil :timeout nil))))))
      (if *print-snark-output*
          (do-prove)
          (with-no-output 
            (do-prove))))))
