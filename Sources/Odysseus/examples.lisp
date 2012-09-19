;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)

(define-relational-fluent is-rested-p)

(define-primitive-action eat '(person))
(define-primitive-action work '(person duration))
(define-primitive-action sleep '(person duration))
(define-primitive-action celebrate '(person)
  :precondition '(iff (poss (celebrate ?p.person) ?s.situation)
                  (is-rested-p ?p.person ?s.situation)))

(declare-primitive-action 'eat (default-interpreter))
(declare-primitive-action 'work (default-interpreter))
(declare-primitive-action 'sleep (default-interpreter))
(declare-primitive-action 'celebrate (default-interpreter))

(defun run-example-0 ()
  (interpret-and-print
   '(celebrate annabelle)))

(defun run-example-0a ()
  (interpret-and-print
   '(seq 
     (celebrate annabelle))))

(defun run-example-0b ()
  (interpret-and-print
   '(seq
     (no-operation)
     (celebrate annabelle))))

(defun run-example-1 ()
  (interpret-and-print 
   '(seq
     (eat lenz)
     (sleep lenz)
     (celebrate lenz))))

(defparameter *program-1*
  '(seq
    (eat ?p.person)
    (sleep ?p.person)
    (celebrate ?p.person)))

(defun run-example-2 ()
  (interpret-and-print *program-1*))

(defun run-example-3 ()
  (interpret-and-print *program-1* :test 'print-everything))

(defparameter *program-2*
  '(seq
    (eat ?p.person)
    (sleep ?p.person)
    (no-operation)
    (celebrate ?p.person)))

(defun run-example-4 ()
  (interpret-and-print *program-2*))

(defun run-example-5 ()
  (interpret-and-print *program-2* :test 'print-everything))

(defun run-example-6 ()
  (interpret-and-print
   '(seq
     (celebrate ?p.person))))

(defun run-example-7 ()
  (interpret-and-print
   '(seq
     (work ?p.person)
     (celebrate ?p.person)))
    :error-value :nobody-can-work-and-celebrate)

(defun run-example-8 ()
  (interpret-and-print
   '(seq
     (work ?p.person)
     (no-operation)
     (celebrate ?p.person))
   :error-value :nobody-can-work-and-celebrate))

(defun run-example-9 ()
  (interpret-and-print
   '(seq
     (work ?p.person)
     (sleep annabelle)
     (celebrate ?p.person))))


(defun run-all-examples ()
  (mapcar (lambda (fun)
            (format t "~&Example: ~A" fun)
            (format t "~&Result: ~:W~2%" (funcall fun)))
          '(run-example-0 run-example-0a run-example-0b
	    run-example-1 run-example-2 run-example-3
            run-example-4 run-example-5 run-example-6
            run-example-7 run-example-8 run-example-9))
  :all-examples-completed)
