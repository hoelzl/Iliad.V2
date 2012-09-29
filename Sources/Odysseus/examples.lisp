;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

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
    (declare-unique-constant 's0 :sort 'situation)
    (declare-unique-function 'do 2
                             :sort '(situation action situation)
                             :injective t)

    ;; Objects
    (declare-subsort 'person 'object)

    (declare-unique-constant 'annabelle :sort 'person)
    (declare-unique-constant 'laith :sort 'person)
    (declare-unique-constant 'lenz :sort 'person)
    (declare-unique-constant 'matthias :sort 'person)
    
    ;; Fluent
    (declare-relational-fluent 'is-rested-p '(person situation))

    ;; Actions
    (declare-primitive-action 'eat '(action person))
    (declare-primitive-action 'work '(action person))
    (declare-primitive-action 'sleep '(action person))
    (declare-primitive-action 'celebrate '(action person)
      :precondition '(iff (poss (celebrate ?p.person) ?s.situation)
                          (is-rested-p ?p.person ?s.situation)))
  
    (declare-unique-function 'no-operation 0
                             :sort '(action)
                             :injective t)
  
    ;; Axioms
    (assert  ',*is-rested-p-axiom*
             :supported nil :sequential t)
    (assert-rewrite ',*is-rested-p-axiom*)
  
    ;; Initial situation
    (assert '(is-rested-p annabelle s0)
            :name :annabelle-is-rested-in-s0)
    (assert '(is-rested-p laith s0)
            :name :laith-is-rested-in-s0)
    (assert '(not (is-rested-p lenz s0))
            :name :lenz-is-not-rested-in-s0)
    
    ;; Background knowledge
    (declare-relation 'female 1 :sort '(person))
    (declare-relation 'male 1 :sort '(person))
    (assert '(iff (female ?p.person) (not (male ?p.person)))
            :supported nil)
    (assert '(or (male ?p.person) (female ?p.person))
            :supported nil)
    (assert '(female annabelle))
    (assert '(male laith))
    (assert '(male lenz))
    (assert '(male matthias))
    ))

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
  (search
   (work ?p.person)
   (celebrate ?p.person)))

(defexample interpret-06a (:set-up-function 'set-up-ewsc-theory
                           :hidden? t
                          :keys '(:error-value :nobody-can-work-and-celebrate))
  (seq
   (work ?p.person)
   (celebrate ?p.person)))

(defexample interpret-06b (:set-up-function 'set-up-ewsc-theory
                           :hidden? t
                          :keys '(:error-value :nobody-can-work-and-celebrate))
  (search
   (seq
    (work ?p.person)
    (celebrate ?p.person))))

(defexample interpret-07 (:set-up-function 'set-up-ewsc-theory)
  (search
   (work ?p.person)
   (eat ?p.person)
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
  (search
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09b (:set-up-function 'set-up-ewsc-theory)
  (search
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09c (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (seq
   (eat ?p.person)
   (eat ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09d (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (search
   (eat ?p.person)
   (work ?p.person)
   (eat ?p.person)
   (sleep ?p.person)
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09e (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (search
   (choose
    (work ?p.person)
    (eat ?p.person)
    (sleep ?p.person))
   (eat ?p.person)
   (celebrate ?p.person)))

(defexample interpret-09f (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
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

(defexample interpret-09g (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
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
   (celebrate ?p.person)))

(defexample interpret-09h (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
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
   (celebrate ?p.person)))

(defexample interpret-09i (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
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
   (celebrate ?p.person)))

(defexample interpret-09j (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (search
   (choose
    (work ?p.person)
    (eat ?p.person)
    (sleep ?p.person))
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

(defexample interpret-12a (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (search
   (choose
    (celebrate ?p.person)
    (seq
     (sleep ?p.person)
     (celebrate lenz)))))

(defexample interpret-12b (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (choose
   (celebrate ?p.person)
   (seq
    (sleep ?p.person)
    (celebrate lenz))))

(defexample interpret-13 (:set-up-function 'set-up-ewsc-theory)
  (seq
   (sleep matthias)
   (search
    (choose
     (work ?p.person)
     (eat ?p.person)
     (sleep ?p.person))
    (eat ?p.person)
    (celebrate ?p.person))))

(defexample interpret-13a (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (seq
   (sleep matthias)
   (search
    (work ?p.person)
    (eat ?p.person)
    (celebrate ?p.person))))

(defexample interpret-13b (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (seq
   (sleep matthias)
   (search
    (eat ?p.person)
    (eat ?p.person)
    (celebrate ?p.person))))

(defexample interpret-13c (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (seq
   (sleep matthias)
   (search
    (sleep ?p.person)
    (eat ?p.person)
    (celebrate ?p.person))))

(defexample interpret-14 (:set-up-function 'set-up-ewsc-theory)
  (holds? '(male ?p.person))
  (sleep ?p.person))

(defexample interpret-14a (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (holds? '(male lenz))
  (sleep ?p.person))

(defexample interpret-14b (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (holds? '(male annabelle))
  (sleep ?p.person))

(defexample interpret-15 (:set-up-function 'set-up-ewsc-theory)
  (search
   (choose (holds (male ?p.person)) (holds (female ?p.person)))
   (celebrate ?p.person)))

(defexample interpret-16 (:set-up-function 'set-up-ewsc-theory)
  (search
   (work annabelle)
   (choose (holds (female ?p.person))
           (holds (male ?p.person)))
   (celebrate ?p.person)
   (holds (male ?p.person))))

(defexample interpret-16a (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (search
   (choose (holds (female ?p.person))
           (holds (male ?p.person)))
   (celebrate ?p.person)
   (holds (male ?p.person))))

(defexample interpret-16b (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (holds (is-rested-p laith s0)))

(defexample interpret-16c (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (holds (is-rested-p laith (do (work annabelle) s0))))

(defexample interpret-16d (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (holds
   (or (= (work annabelle) (sleep laith))
       (and (is-rested-p laith s0)
            (not (= (work annabelle) (work laith)))))))

(defexample interpret-16d.1 (:set-up-function 'set-up-ewsc-theory
                             :hidden? t)
  (holds
   (= (work annabelle) (sleep laith))))

(defexample interpret-16d.2 (:set-up-function 'set-up-ewsc-theory
                             :hidden? t)
  (holds
   (not (= (work annabelle) (work laith)))))

(defexample interpret-16e (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (holds (poss (celebrate laith) (do (work annabelle) s0))))

(defexample interpret-16f (:set-up-function 'set-up-ewsc-theory
                           :hidden? t)
  (holds (= (celebrate laith) (work ?p.person))))



