;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-snark)

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
  (print-rows-when-finished nil)
  (print-agenda-when-finished t)
  (run-time-limit 0.1)
  (use-conditional-answer-creation t)
  ;; (use-subsumption-by-false nil)
  (use-constructive-answer-restriction nil)
  (allow-skolem-symbols-in-answers nil)
  )


(defun prove-or-refute (term &rest args
			&key (set-up-theory 'set-up-theory)
			&allow-other-keys)
  (initialize-snark)
  (funcall set-up-theory)
  (let* ((new-args (print (alexandria:remove-from-plist args :set-up-theory)))
	 (result (apply 'prove term new-args)))
    (ecase result
      (:proof-found result)
      ((:run-time-limit :agenda-empty)
       (initialize-snark)
       (funcall set-up-theory)
       (let ((result (apply 'prove `(snark::not ,term) new-args)))
	 (ecase result
	   (:proof-found :refutation-found)
	   (:agenda-empty :agenda-empty)
	   (:run-time-limit result)))))))

(defun ida-prove-or-refute (term &rest args
			    &key (set-up-theory 'set-up-theory)
			    &allow-other-keys)
  (let* ((initial-run-time-limit 0.1)
	 (run-time-limit initial-run-time-limit)
	 (iterations 6))
    (dotimes (i iterations)
      (initialize-snark)
      (run-time-limit run-time-limit)
      (funcall set-up-theory)
      (let ((result (apply 'prove-or-refute term args)))
	(if (eql result :run-time-limit)
	    (setf run-time-limit (* 2 run-time-limit))
	    (return-from ida-prove-or-refute result))))
    (run-time-limit initial-run-time-limit)
    :run-time-limit))

(defvar *print-snark-output* nil)

(defun prove-using-snark (term &rest args &key set-up-theory answer)
  (declare (ignore set-up-theory answer))
  (flet ((do-prove ()
	   (let ((result (apply 'ida-prove-or-refute term args)))
	     (case result
	       (:proof-found
		(values t :proof-found (answer)))
	       (:refutation-found
		(values nil :refutation-found (answer)))
	       (otherwise
		(values nil :timeout nil))))))
    (if *print-snark-output*
	(do-prove)
	(with-no-output 
	  (do-prove)))))