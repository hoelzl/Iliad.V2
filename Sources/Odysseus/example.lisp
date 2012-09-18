;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)

(define-primitive-action eat)
(define-primitive-action sleep)
(define-primitive-action celebrate)

(declare-primitive-action 'eat (default-interpreter))
(declare-primitive-action 'sleep (default-interpreter))
(declare-primitive-action 'celebrate (default-interpreter))

(defun run-example-1 ()
  (interpret-and-print 
   '(seq
     (eat something)
     (sleep several hours)
     (celebrate))))

(defparameter *program-1*
  '(seq
    (eat ?x things)
    (no-operation)
    (no-operation)
    (sleep ?x hours)
    (no-operation)
    (no-operation)
    (celebrate)))

(defun run-example-2 ()
  (interpret-and-print *program-1*))

(defun run-example-3 ()
  (interpret-and-print *program-1* :test 'print-everything))

(defun run-all-examples ()
  (mapcar (lambda (fun)
            (format t "~&Result: ~A~2%" (funcall fun)))
          '(run-example-1 run-example-2 run-example-3))
  :all-examples-completed)

