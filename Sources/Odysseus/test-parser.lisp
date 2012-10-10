;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-parser-suite)

(deftest test-starts-with-question-mark-p ()
  (is (starts-with-question-mark-p '?x))
  (is (starts-with-question-mark-p '?a-very-long-variable-name))
  (is (not (starts-with-question-mark-p '?)))
  (is (not (starts-with-question-mark-p 'foo))))

(defun test-parse-unary-connective (term operator cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (is (typep parsed-term (term-type-for-operator operator cc)))
    (is (eql operator (operator parsed-term)))
    (is (typep (arguments parsed-term) 'cons))
    (is (= 1 (length (arguments parsed-term))))
    (let ((x (argument parsed-term)))
      (is (typep x 'variable-term))
      (is (eql 'x (name x)))))
  (signals simple-error
    (parse-into-term-representation (butlast term) cc))
  (signals simple-error
    (parse-into-term-representation (append term '(?z)) cc)))

(defun test-parse-binary-connective (term operator cc &key exactly-two-terms-allowed)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (is (typep parsed-term (term-type-for-operator operator cc)))
    (is (eql operator (operator parsed-term)))
    (is (typep (arguments parsed-term) 'cons))
    (is (= 2 (length (arguments parsed-term))))
    (destructuring-bind (x y) (arguments parsed-term)
      (is (typep x 'variable-term))
      (is (eql 'x (name x)))
      (is (typep y 'variable-term))
      (is (eql 'y (name y)))))
  (when exactly-two-terms-allowed
    (signals simple-error
      (parse-into-term-representation (butlast term) cc))
    (signals simple-error
      (parse-into-term-representation (append term '(?z)) cc))))

(defun test-parse-quantification-1 (term operator cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (is (typep parsed-term (term-type-for-operator operator cc)))
    (is (eql operator (operator parsed-term)))
    (destructuring-bind (x) (bound-variables parsed-term)
        (is (typep x 'variable-term))
        (is (eql 'x (name x)))
        (is (= 1 (length (bound-variables parsed-term))))
        (is (eql x (first (bound-variables parsed-term))))
        (let ((arg (argument parsed-term)))
          (is (eql 'unknown-general-application-term (type-of arg)))
          (is (eql x (first (arguments arg))))))
    (is (typep (arguments parsed-term) 'cons))
    (is (= 1 (length (arguments parsed-term)))))
  (signals simple-error
    (parse-into-term-representation (butlast term) cc))
  (signals simple-error
    (parse-into-term-representation (append term '((g ?z))) cc)))

(defun test-parse-quantification-2 (term operator cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (is (typep parsed-term (term-type-for-operator operator cc)))
    (is (eql operator (operator parsed-term)))
    (destructuring-bind (x y) (bound-variables parsed-term) 
        (is (typep x 'variable-term))
        (is (typep y 'variable-term))
        (is (eql 'x (name x)))
        (is (eql 'y (name y)))
        (is (= 2 (length (bound-variables parsed-term))))
        (is (eql x (first (bound-variables parsed-term))))
        (is (eql y (second (bound-variables parsed-term))))
        (let ((arg (argument parsed-term)))
          (is (eql 'unknown-general-application-term (type-of arg)))
          (is (eql x (first (arguments arg))))
          (is (eql y (second (arguments arg))))))
    (is (typep (arguments parsed-term) 'cons))
    (is (= 1 (length (arguments parsed-term)))))
  (signals simple-error
    (parse-into-term-representation (butlast term) cc))
  (signals simple-error
    (parse-into-term-representation (append term '((g ?z))) cc)))

(deftest test-parse-into-term-representation-01 ()
  (let ((cc (make-instance 'compilation-unit)))
    (let ((foo (parse-into-term-representation '?foo cc)))
      (is (typep foo 'variable-term))
      (is (eql 'foo (name foo))))
    (let ((bar (parse-into-term-representation 'bar cc)))
      (is (typep bar 'primitive-term))
      (is (eql 'bar (value bar))))
    (let ((num (parse-into-term-representation 123 cc)))
      (is (typep num 'number-term))
      (is (eql 123 (value num))))
    (let ((foo (parse-into-term-representation '(foo ?x ?y) cc)))
      (is (typep foo 'unknown-general-application-term))
      (is (eql 'foo (operator foo)))
      (is (typep (arguments foo) 'cons))
      (is (= 2 (length (arguments foo))))
      (destructuring-bind (x y) (arguments foo)
        (is (typep x 'variable-term))
        (is (eql 'x (name x)))
        (is (typep y 'variable-term))
        (is (eql 'y (name y)))))))

(deftest test-parse-into-term-representation-02 ()
  (let ((cc (make-instance 'compilation-unit)))
    (test-parse-unary-connective '(not ?x) 'not cc)
    (test-parse-unary-connective '(~ ?x) 'not cc)
    (test-parse-binary-connective '(and ?x ?y) 'and cc)
    (test-parse-binary-connective '(& ?x ?y) 'and cc)
    (test-parse-binary-connective '(|,| ?x ?y) 'and cc)
    (test-parse-binary-connective '(or ?x ?y) 'or cc)
    (test-parse-binary-connective '(|;| ?x ?y) 'or cc)
    (test-parse-binary-connective '(implies ?x ?y) 'implies cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(-> ?x ?y) 'implies cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(=> ?x ?y) 'implies cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(implied-by ?x ?y) 'implied-by cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(<- ?x ?y) 'implied-by cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(<= ?x ?y) 'implied-by cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(is-implied-by ?x ?y) 'implied-by cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(iff ?x ?y) 'iff cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(<-> ?x ?y) 'iff cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(<=> ?x ?y) 'iff cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(equiv ?x ?y) 'iff cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(equivalent ?x ?y) 'iff cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(is-equivalent ?x ?y) 'iff cc
                                  :exactly-two-terms-allowed t)
    (test-parse-binary-connective '(are-equivalent ?x ?y) 'iff cc
                                  :exactly-two-terms-allowed t)
    (test-parse-quantification-1 '(foreach ?x (f ?x)) 'forall cc)
    (test-parse-quantification-1 '(each ?x (f ?x)) 'forall cc)
    (test-parse-quantification-1 '(forall ?x (f ?x)) 'forall cc)
    (test-parse-quantification-1 '(exists ?x (f ?x)) 'exists cc)
    (test-parse-quantification-1 '(exist ?x (f ?x)) 'exists cc)
    (test-parse-quantification-2 '(foreach (?x ?y) (f ?x ?y))  'forall cc)
    (test-parse-quantification-2 '(exists (?x ?y) (f ?x ?y))  'exists cc)
    (test-parse-quantification-2 '(foreach ((?x) (?y)) (f ?x ?y))  'forall cc)
    (test-parse-quantification-2 '(exists ((?x) (?y)) (f ?x ?y))  'exists cc)))


(defun test-parse-empty-program-term (term cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (is (typep parsed-term 'empty-program-term))))

(deftest test-parse-into-term-representation-03 ()
  (let ((cc (make-instance 'compilation-unit)))
    (test-parse-empty-program-term 'nil cc)
    (test-parse-empty-program-term 'null cc)))

(defun test-parse-primitive-action-term (term action-name action-class arity cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (is (typep parsed-term action-class))
    (is (eql action-name (operator parsed-term)))
    (is (consp (arguments parsed-term)))
    (is (= arity (length (arguments parsed-term))))
    (is (typep (first (arguments parsed-term)) 'variable-term))))

(define-primitive-action 'foo '(t t))
(define-primitive-action 'bar '(t))

(deftest test-parse-into-term-representation-04 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (test-parse-primitive-action-term
     '(foo ?x ?y) 'foo 'foo-term 2 cc)))

(deftest test-parse-into-term-representation-05 ()
  (let* ((cc (make-instance 'compilation-unit))
         (parsed-term (parse-into-term-representation '(test (and (f ?x) (g ?x))) cc)))
    (is (typep parsed-term 'test-term))
    (is (eql 'holds? (operator parsed-term)))
    (is (typep (argument parsed-term) 'conjunction-term))
    (destructuring-bind (f g) (arguments (argument parsed-term))
      (is (typep f 'unknown-general-application-term))
      (is (eql 'f (operator f)))
      (is (typep g 'unknown-general-application-term))
      (is (eql 'g (operator g))))))
    

(deftest test-parse-into-term-representation-06 ()
  (let* ((cc (make-instance 'compilation-unit))
         (parsed-term (parse-into-term-representation '(seq (f ?x) (g ?x)) cc)))
    (is (typep parsed-term 'sequence-term))
    (is (eql 'seq (operator parsed-term)))
    (is (= 2 (length (body parsed-term))))
    (destructuring-bind (f g) (body parsed-term)
      (is (typep f 'unknown-general-application-term))
      (is (eql 'f (operator f)))
      (is (typep g 'unknown-general-application-term))
      (is (eql 'g (operator g))))))
    

(deftest test-parse-into-term-representation-07 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(seq (foo ?x) (g ?x)) cc)))
      (is (typep parsed-term 'sequence-term))
      (is (eql 'seq (operator parsed-term)))
      (is (= 2 (length (body parsed-term))))
      (destructuring-bind (foo g) (body parsed-term)
        (is (typep foo 'foo-term))
        (is (eql 'foo (operator foo)))
        (is (typep g 'unknown-general-application-term))
        (is (eql 'g (operator g)))))))

(deftest test-parse-into-term-representation-08 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (declare-primitive-action 'bar cc)
    (let ((parsed-term (parse-into-term-representation
                        '(one-of (foo north) (foo south) (bar west) (bar east))
                        cc)))
      (is (typep parsed-term 'action-choice-term))
      (is (eql 'one-of (operator parsed-term))))))

(deftest test-parse-into-term-representation-09 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation
                        '(pick ?x (foo ?x))
                        cc)))
      (is (typep parsed-term 'argument-choice-term))
      (is (eql 'pick (operator parsed-term))))))

(deftest test-parse-into-term-representation-10 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (declare-primitive-action 'bar cc)
    (let ((parsed-term (parse-into-term-representation
                        '(repeat (foo north))
                        cc)))
      (is (typep parsed-term 'iteration-term))
      (is (eql 'repeat (operator parsed-term))))))

(deftest test-parse-into-term-representation-11 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (declare-primitive-action 'bar cc)
    (let ((parsed-term (parse-into-term-representation
                        '(if (foo north) (bar east) (bar west))
                        cc)))
      (is (typep parsed-term 'conditional-term))
      (is (eql 'if (operator parsed-term)))
      (is (typep (arg1 parsed-term) 'foo-term))
      (let ((north-arg (first (arguments (arg1 parsed-term)))))
        (is (typep north-arg 'primitive-term))
        (is (eql 'north (value north-arg))))
      (is (typep (arg2 parsed-term) 'bar-term))
      (let ((east-arg (first (arguments (arg2 parsed-term)))))
        (is (typep east-arg 'primitive-term))
        (is (eql 'east (value east-arg))))
      (is (typep (arg3 parsed-term) 'bar-term))
      (let ((west-arg (first (arguments (arg3 parsed-term)))))
        (is (typep west-arg 'primitive-term))
        (is (eql 'west (value west-arg)))))))

(deftest test-parse-into-term-representation-12 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation
                        '(while (bar ?x) (foo north))
                        cc)))
      (is (typep parsed-term 'while-loop-term))
      (is (eql 'while (operator parsed-term))))))

(deftest test-parse-into-term-representation-13 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(search (foo ?x) (g ?x)) cc)))
      (is (typep parsed-term 'search-term))
      (is (eql 'search (operator parsed-term)))
      (is (= 2 (length (body parsed-term))))
      (destructuring-bind (foo g) (body parsed-term)
        (is (typep foo 'foo-term))
        (is (eql 'foo (operator foo)))
        (is (typep g 'unknown-general-application-term))
        (is (eql 'g (operator g)))))))

(deftest test-parse-into-term-representation-14 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(concurrently (foo ?x) (g ?x)) cc)))
      (is (typep parsed-term 'concurrent-term))
      (is (eql 'concurrently (operator parsed-term)))
      (is (= 2 (length (arguments parsed-term))))
      (destructuring-bind (foo g) (arguments parsed-term)
        (is (typep foo 'foo-term))
        (is (eql 'foo (operator foo)))
        (is (typep g 'unknown-general-application-term))
        (is (eql 'g (operator g)))))))

(deftest test-parse-into-term-representation-15 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(prioritized (foo ?x) (g ?x)) cc)))
      (is (typep parsed-term 'prioritized-concurrent-term))
      (is (eql 'prioritized (operator parsed-term)))
      (is (= 2 (length (arguments parsed-term))))
      (destructuring-bind (foo g) (arguments parsed-term)
        (is (typep foo 'foo-term))
        (is (eql 'foo (operator foo)))
        (is (typep g 'unknown-general-application-term))
        (is (eql 'g (operator g)))))))

(define-functional-fluent 'robot-position '(position robot))
(define-relational-fluent 'holds-item '(robot))

(deftest test-parse-into-term-representation-16 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-functional-fluent 'holds-item cc)
    (let ((ff (parse-into-term-representation '(holds-item ?robot ?item) cc)))
      (is (typep ff 'holds-item-term))
      (is (typep ff 'known-general-application-term))
      (is (= 2 (length (arguments ff)))))))


(deftest test-parse-into-term-representation-17 ()
  (let ((cc (make-instance 'compilation-unit)))
    (declare-relational-fluent 'robot-position cc)
    (let ((rf (parse-into-term-representation '(robot-position ?robot) cc)))
      (is (typep rf 'robot-position-term))
      (is (typep rf 'known-general-application-term))
      (is (= 1 (length (arguments rf)))))))

(deftest test-parse-into-term-representation-18 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation '(declare-sort 'situation) cc)))
    (is (typep term 'sort-declaration-term))
    (is (eql 'situation (name term)))
    (is (eql 'situation (declared-sort term cc)))))

(deftest test-parse-into-term-representation-19 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation
                '(declare-subsort 'work-action 'action) cc)))
    (is (typep term 'sort-declaration-term))
    (is (eql 'work-action (name term)))
    (is (eql 'work-action (declared-sort term cc)))
    (is (eql 'action (supersort term)))))

(deftest test-parse-into-term-representation-20 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation
                '(declare-sorts-incompatible 'situation 'action 'object) cc)))
    (is (typep term 'sorts-incompatible-declaration-term))
    (is (equalp '(situation action object) (sorts term)))))

(deftest test-parse-into-term-representation-21 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation
                '(declare-constant 's0 :sort 'situation) cc)))
    (is (typep term 'constant-declaration-term))
    (is (eql 's0 (name term)))
    (is (eql 'situation (declared-sort term cc)))))

(deftest test-parse-into-term-representation-22 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation
                '(declare-function 'do 2
		    :sort '(situation action situation)
		    :injective t)
                cc)))
    (is (typep term 'function-declaration-term))
    (is (eql 'do (name term)))
    (is (equalp '(situation action situation) (declared-sort term cc)))))

(deftest test-parse-into-term-representation-23 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation
                '(declare-relation 'is-rested-p 2
                  :sort '(person situation))
                cc)))
    (is (typep term 'relation-declaration-term))
    (is (eql 'is-rested-p (name term)))
    (is (equalp '(person situation) (declared-sort term cc)))))

(deftest test-parse-into-term-representation-24 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation
                '(declare-ordering-greaterp 'do 'work 'sleep
                                            'annabelle 'lenz 'matthias)
                cc)))
    (is (typep term 'ordering-declaration-term))
    (is (equalp '(do work sleep annabelle lenz matthias)
                    (ordered-symbols term)))))


(deftest test-parse-into-term-representation-25 ()
  (let* ((cc (make-instance 'compilation-unit))
         (term (parse-into-term-representation
                '(assert '(is-rested-p annabelle s0)
                 :name :annabelle-is-rested-in-s0)
                cc)))
    (is (typep term 'logical-sentence-declaration-term))
    (is (typep term 'logical-assertion-term))
    (is (equalp '(:name :annabelle-is-rested-in-s0)
                    (keywords term)))))

