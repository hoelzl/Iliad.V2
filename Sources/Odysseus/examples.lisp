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

(defexample interpret-01 ()
  (celebrate annabelle))

(defexample interpret-01a ()
  (seq 
   (celebrate annabelle)))

(defexample interpret-01b ()
  (seq
   (no-operation)
   (celebrate annabelle)))

(defexample interpret-01c ()
  (celebrate lenz))

(defexample interpret-02 ()
  (seq
   (eat lenz)
   (sleep lenz)
   (celebrate lenz)))

(defexample interpret-02a ()
  (eat lenz)
  (sleep lenz)
  (celebrate lenz))

(defexample interpret-03 ()
  (seq
   (eat ?p.person)
   (sleep ?p.person)
   (celebrate ?p.person)))

(defexample interpret-04 ()
  (seq
   (eat ?p.person)
   (sleep ?p.person)
   (no-operation)
   (celebrate ?p.person)))

(defexample interpret-05 ()
  (seq
   (celebrate ?p.person)))

(defexample interpret-06 (:error-value :nobody-can-work-and-celebrate)
  (seq
   (work ?p.person)
   (celebrate ?p.person)))

(defexample interpret-06a (:error-value :nobody-can-work-and-celebrate)
  (search
   (work ?p.person)
   (celebrate ?p.person)))

(defexample interpret-07 (:error-value :nobody-can-work-and-celebrate)
  (seq
   (work ?p.person)
   (no-operation)
   (celebrate ?p.person)))

(defexample interpret-08 ()
  (seq
   (work ?p.person)
   (sleep annabelle)
   (celebrate ?p.person)))

(defexample interpret-09 ()
  (search
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09a ()
  (seq
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09b ()
  (search
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09c ()
  (search
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09d ()
  (search
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-10 ()
  (choose
   (sleep ?p.person)
   (celebrate ?p.person)))

(defexample interpret-11 ()
  (search
   (choose
    (celebrate ?p.person)
    (seq
     (sleep ?p.person)
     (celebrate ?p.person)))))

(defexample interpret-12 ()
  (search
   (choose
    (celebrate ?p.person)
    (seq
     (sleep ?p.person)
     (celebrate ?p.person)))))

(defexample interpret-12a ()
  (search
   (choose
    (celebrate ?p.person)
    (seq
     (sleep ?p.person)
     (celebrate lenz)))))

(defexample interpret-12b ()
  (choose
   (celebrate ?p.person)
   (seq
    (sleep ?p.person)
    (celebrate lenz))))
