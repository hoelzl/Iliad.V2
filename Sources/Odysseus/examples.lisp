;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)

;; TODO: Generate DECLARE-FUNCTION calls from DECLARE-FUNCTIONAL-FLUENT
;;       Generate DECLARE-RELATION calls from DELCARE-RELATIONAL-FLUENT
;;       Generate unique names axioms

(defvar *is-rested-p-axiom*
  '(forall ((p :sort person)
            (a :sort action)
            (s :sort situation))
    (iff (is-rested-p p (do a s))
     (or ;; (and (not (is-rested p s)) (= a (sleep p)))
      (= a (sleep p))
      (and (is-rested-p p s)
       (not (= a (work p))))))))

(defun set-up-ewsc-theory ()
  `(seq
    (declare-sort 'situation)
    (declare-sort 'action)
    (declare-sort 'object)
    (declare-sorts-incompatible 'situation 'action 'object)
  
    ;; Situations
    (declare-constant 's0 :sort 'situation)
    (declare-function 'do 2
                      :sort '(situation action situation)
  		      :injective t)

    ;; Objects
    (declare-subsort 'person 'object)
    (declare-constant 'annabelle :sort 'person :constructor nil)
    (declare-constant 'lenz :sort 'person :constructor nil)
    (declare-constant 'matthias :sort 'person :constructor nil)
    (assert '(not (= annabelle lenz))
                   :supported nil)
    (assert '(not (= annabelle matthias))
                   :supported nil)
    (assert '(not (= lenz matthias))
                   :supported nil)
    
    ;; Fluent
    (declare-relational-fluent 'is-rested-p '(person situation))
    (declare-relation 'is-rested-p 2
                      :sort '(person situation))

    (declare-primitive-action 'eat '(action person))
    (declare-primitive-action 'work '(action person duration))
    (declare-primitive-action 'sleep '(action person duration))
    (declare-primitive-action 'celebrate '(action person)
      :precondition '(iff (poss (celebrate ?p.person) ?s.situation)
                          (is-rested-p ?p.person ?s.situation)))
  
    ;; Actions
    (declare-function 'work 1
  		    :sort '(action person)
  		    :injective t)
    (declare-function 'sleep 1
  		    :sort '(action person)
  		    :injective t)
    (declare-function 'celebrate 1
                      :sort '(action person)
                      :injective t)
    (declare-function 'eat 1
                      :sort '(action person)
                      :injective t)
    (declare-function 'no-operation 0
                      :sort '(action)
                      :injective t)
    (assert '(forall ((p1 :sort person)
                      (p2 :sort person))
                     (not (= (work p2) (sleep p1)))))
    (assert '(forall ((p1 :sort person)
                      (p2 :sort person))
                     (not (= (work p2) (eat p1)))))
    (assert '(forall ((p1 :sort person)
                      (p2 :sort person))
                     (not (= (work p2) (celebrate p1)))))
    (assert '(forall ((p1 :sort person)
                      (p2 :sort person))
                     (not (= (eat p2) (sleep p1)))))
    (assert '(forall ((p1 :sort person)
                      (p2 :sort person))
                     (not (= (eat p2) (celebrate p1)))))
    (assert '(forall ((p1 :sort person)
                      (p2 :sort person))
                     (not (= (sleep p2) (celebrate p1)))))
    (assert '(forall ((p :sort person))
                     (not (= (no-operation) (sleep p)))))
    (assert '(forall ((p :sort person))
                     (not (= (no-operation) (work p)))))
    (assert '(forall ((p :sort person))
                     (not (= (no-operation) (eat p)))))
    (assert '(forall ((p :sort person))
                     (not (= (no-operation) (celebrate p)))))
  
    (declare-ordering-greaterp 'do 'work 'sleep 'annabelle 'lenz 'matthias)
    
    ;; Axioms
    (assert  ',*is-rested-p-axiom*
             :supported nil :sequential t)
    (assert-rewrite ',*is-rested-p-axiom*)
  
    (assert '(is-rested-p annabelle s0)
            :name :annabelle-is-rested-in-s0)
    (assert '(not (is-rested-p lenz s0))
            :name :lenz-is-not-rested-in-s0)
  
    (assert '(iff (odysseus-syntax:poss (celebrate ?p.person) ?s.situation)
                  (is-rested-p ?p.person ?s.situation)))))

#+(or)
(define-procedure control (x.person y.person s.situation)
  (celebrate lenz))

(defexample interpret-01 (:set-up-function 'set-up-ewsc-theory)
  (celebrate annabelle))

(defexample interpret-01a (:set-up-function 'set-up-ewsc-theory)
  (seq 
   (celebrate annabelle)))

(defexample interpret-01b (:set-up-function 'set-up-ewsc-theory)
  (seq
   (no-operation)
   (celebrate annabelle)))

(defexample interpret-01c (:set-up-function 'set-up-ewsc-theory)
  (celebrate lenz))

(defexample interpret-01d (:set-up-function 'set-up-ewsc-theory)
  (celebrate matthias))

(defexample interpret-02 (:set-up-function 'set-up-ewsc-theory)
  (seq
   (eat lenz)
   (sleep lenz)
   (celebrate lenz)))

(defexample interpret-02a (:set-up-function 'set-up-ewsc-theory)
  (eat lenz)
  (sleep lenz)
  (celebrate lenz))

(defexample interpret-03 (:set-up-function 'set-up-ewsc-theory)
  (seq
   (eat ?p.person)
   (sleep ?p.person)
   (celebrate ?p.person)))

(defexample interpret-03a (:set-up-function 'set-up-ewsc-theory)
  (seq
   (celebrate ?p.person)
   (eat ?p.person)
   (sleep ?p.person)))

(defexample interpret-04 (:set-up-function 'set-up-ewsc-theory)
  (seq
   (eat ?p.person)
   (sleep ?p.person)
   (no-operation)
   (celebrate ?p.person)))

(defexample interpret-05 (:set-up-function 'set-up-ewsc-theory)
  (seq
   (celebrate ?p.person)))

(defexample interpret-06 (:set-up-function 'set-up-ewsc-theory
                          :keys '(:error-value :nobody-can-work-and-celebrate))
  (seq
   (work ?p.person)
   (celebrate ?p.person)))

(defexample interpret-06a (:set-up-function 'set-up-ewsc-theory
                          :keys '(:error-value :nobody-can-work-and-celebrate))
  (search
   (work ?p.person)
   (celebrate ?p.person)))

(defexample interpret-06b (:set-up-function 'set-up-ewsc-theory
                          :keys '(:error-value :nobody-can-work-and-celebrate))
  (search
   (seq
    (work ?p.person)
    (celebrate ?p.person))))

(defexample interpret-07 (:set-up-function 'set-up-ewsc-theory
                          :keys '(:error-value :nobody-can-work-and-celebrate))
  (seq
   (work ?p.person)
   (no-operation)
   (celebrate ?p.person)))

(defexample interpret-08 (:set-up-function 'set-up-ewsc-theory)
  (seq
   (work ?p.person)
   (sleep annabelle)
   (celebrate ?p.person)))

(defexample interpret-09 (:set-up-function 'set-up-ewsc-theory)
  (search
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09a (:set-up-function 'set-up-ewsc-theory)
  (seq
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09b (:set-up-function 'set-up-ewsc-theory)
  (search
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09c (:set-up-function 'set-up-ewsc-theory)
  (search
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09d (:set-up-function 'set-up-ewsc-theory)
  (search
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09e (:hidden? t)
  (search
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09f (:hidden? t)
  (search
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09g (:hidden? t)
  (search
   (sleep ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))


(defexample interpret-09h (:hidden? t)
  (search
   (work ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-10 (:set-up-function 'set-up-ewsc-theory)
  (choose
   (sleep ?p.person)
   (celebrate ?p.person)))

(defexample interpret-11 (:set-up-function 'set-up-ewsc-theory)
  (search
   (choose
    (celebrate ?p.person)
    (seq
     (sleep ?p.person)
     (celebrate ?p.person)))))

(defexample interpret-12 (:set-up-function 'set-up-ewsc-theory)
  (search
   (choose
    (celebrate ?p.person)
    (seq
     (sleep ?p.person)
     (celebrate ?p.person)))))

(defexample interpret-12a (:set-up-function 'set-up-ewsc-theory)
  (search
   (choose
    (celebrate ?p.person)
    (seq
     (sleep ?p.person)
     (celebrate lenz)))))

(defexample interpret-12b (:set-up-function 'set-up-ewsc-theory)
  (choose
   (celebrate ?p.person)
   (seq
    (sleep ?p.person)
    (celebrate lenz))))
