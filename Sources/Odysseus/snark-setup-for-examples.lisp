;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

;;; Set up the domain theory for the example.  We need to be in the
;;; ODYSSEUS-SNARK package to do this.

(in-package #:odysseus-snark)

(defparameter *is-rested-p-axiom*
  '(snark::forall ((p :sort person)
                   (a :sort action)
                   (s :sort situation))
    (snark::iff (is-rested-p p (do a s))
         (or ;; (and (snark::not (is-rested p s)) (= a (sleep p)))
             (= a (sleep p))
             (and (is-rested-p p s)
	          (snark::not (= a (work p))))))))

(defun odysseus-snark:set-up-theory ()
  (declare-sort 'situation)
  (declare-sort 'action)
  (declare-sort 'object)
  (declare-sorts-incompatible 'situation 'action 'object)
  (declare-subsort 'work-action 'action)
  (declare-subsort 'sleep-action 'action)
  (declare-subsort 'celebrate-action 'action)
  (declare-subsort 'eat-action 'action)
  (declare-subsort 'no-action 'action)
  (declare-sorts-incompatible 'work-action 'sleep-action
                              'celebrate-action 'eat-action
                              'no-action)

  ;; Situations
  (declare-constant 'odysseus-situation::s0 :sort 'situation :constructor nil)
  (declare-function 'do 2
		    :sort '(situation action situation)
		    :injective t)
  #+(or)
  (assert '(snark::forall ((s :sort situation)
		    (a :sort action))
	    (snark::not (= odysseus-situation::s0 (do a s)))))
  #+(or)
  (assert '(snark::forall ((s1 :sort situation)
		    (s2 :sort situation)
		    (a1 :sort action)
		    (a2 :sort action))
	    (snark::iff (= (do a1 s1) (do a2 s2))
	     (and (= a1 a2) (= s1 s2)))))

  ;; Objects
  (declare-subsort 'person 'object)
  (declare-constant 'annabelle :sort 'person :constructor nil)
  (declare-constant 'lenz :sort 'person :constructor nil)
  (declare-constant 'matthias :sort 'person :constructor nil)
  (snark::assert '(snark::not (= annabelle lenz))
                 :supported nil)
  (snark::assert '(snark::not (= annabelle matthias))
                 :supported nil)
  (snark::assert '(snark::not (= lenz matthias))
                 :supported nil)
  
  ;; Fluent
  (declare-relation 'is-rested-p 2
		    :sort '(person situation))

  ;; Actions
  (declare-function 'work 1
		    :sort '(work-action person)
		    :injective t)
  (declare-function 'sleep 1
		    :sort '(sleep-action person)
		    :injective t)
  (declare-function 'celebrate 1
                    :sort '(celebrate-action person)
                    :injective t)
  (declare-function 'eat 1
                    :sort '(eat-action person)
                    :injective t)
  (declare-function 'no-operation 0
                    :sort '(no-action)
                    :injective t)
  (snark::assert '(snark::forall ((p1 :sort person)
                                  (p2 :sort person))
                   (snark::not (= (work p2) (sleep p1)))))
  (snark::assert '(snark::forall ((p1 :sort person)
                                  (p2 :sort person))
                   (snark::not (= (work p2) (eat p1)))))
  (snark::assert '(snark::forall ((p1 :sort person)
                                  (p2 :sort person))
                   (snark::not (= (work p2) (celebrate p1)))))
  (snark::assert '(snark::forall ((p1 :sort person)
                                  (p2 :sort person))
                   (snark::not (= (eat p2) (sleep p1)))))
  (snark::assert '(snark::forall ((p1 :sort person)
                                  (p2 :sort person))
                   (snark::not (= (eat p2) (celebrate p1)))))
  (snark::assert '(snark::forall ((p1 :sort person)
                                  (p2 :sort person))
                   (snark::not (= (sleep p2) (celebrate p1)))))
  (snark::assert '(snark::forall ((p :sort person))
                   (snark::not (= (no-operation) (sleep p)))))
  (snark::assert '(snark::forall ((p :sort person))
                   (snark::not (= (no-operation) (work p)))))
  (snark::assert '(snark::forall ((p :sort person))
                   (snark::not (= (no-operation) (eat p)))))
  (snark::assert '(snark::forall ((p :sort person))
                   (snark::not (= (no-operation) (celebrate p)))))

  (declare-ordering-greaterp 'do 'work 'sleep 'annabelle 'lenz 'matthias)

  ;; Axioms
  (snark::assert *is-rested-p-axiom*
                 :supported nil :sequential t)
  #+(or)
  (assert *is-rested-p-axiom*)
  (assert-rewrite *is-rested-p-axiom*)

  (snark::assert '(is-rested-p annabelle odysseus-situation::s0)
                 :name :annabelle-is-rested-in-s0)
  (snark::assert '(snark::not (is-rested-p lenz odysseus-situation::s0))
                 :name :lenz-is-not-rested-in-s0)

  (snark::assert '(snark::iff (odysseus-syntax:poss (celebrate ?p.person) ?s.situation)
                   (is-rested-p ?p.person ?s.situation))))

(defun set-up-tests ()
  (initialize-snark)
  (odysseus-snark:set-up-theory))
