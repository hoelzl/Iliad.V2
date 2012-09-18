;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-tests)

#+5am
(5am:in-suite odysseus-parser-suite)

#+5am
(5am:test (test-starts-with-question-mark-p
           :compile-at :definition-time)
  (5am:is-true (starts-with-question-mark-p '?x))
  (5am:is-true (starts-with-question-mark-p '?a-very-long-variable-name))
  (5am:is-false (starts-with-question-mark-p '?))
  (5am:is-false (starts-with-question-mark-p 'foo)))

#+5am
(defun test-parse-unary-connective (term operator cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (5am:is-true (typep parsed-term (term-type-for-operator operator cc)))
    (5am:is (eql operator (operator parsed-term)))
    (5am:is-true (typep (arguments parsed-term) 'cons))
    (5am:is (= 1 (length (arguments parsed-term))))
    (let ((x (argument parsed-term)))
      (5am:is-true (typep x 'variable-term))
      (5am:is (eql '?x (name x)))))
  (5am:signals simple-error
    (parse-into-term-representation (butlast term) cc))
  (5am:signals simple-error
    (parse-into-term-representation (append term '(?z)) cc)))

#+5am
(defun test-parse-binary-connective (term operator cc &key exactly-two-terms-allowed)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (5am:is-true (typep parsed-term (term-type-for-operator operator cc)))
    (5am:is (eql operator (operator parsed-term)))
    (5am:is-true (typep (arguments parsed-term) 'cons))
    (5am:is (= 2 (length (arguments parsed-term))))
    (destructuring-bind (x y) (arguments parsed-term)
      (5am:is-true (typep x 'variable-term))
      (5am:is (eql '?x (name x)))
      (5am:is-true (typep y 'variable-term))
      (5am:is (eql '?y (name y)))))
  (when exactly-two-terms-allowed
    (5am:signals simple-error
      (parse-into-term-representation (butlast term) cc))
    (5am:signals simple-error
      (parse-into-term-representation (append term '(?z)) cc))))

#+5am
(defun test-parse-quantification-1 (term operator cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (5am:is-true (typep parsed-term (term-type-for-operator operator cc)))
    (5am:is (eql operator (operator parsed-term)))
    (destructuring-bind (xb) (bindings parsed-term) 
      (5am:is-true (typep xb 'binding))
      (let ((x (binding-variable xb)))
        (5am:is-true (typep x 'variable-term))
        (5am:is (eql '?x (name x)))
        (5am:is (eql nil (slot-value parsed-term 'bound-variables)))
        (5am:is (= 1 (length (bound-variables parsed-term))))
        ;; Check that we cache the result of bound-variables
        (5am:is (not (eql nil (slot-value parsed-term 'bound-variables))))
        (5am:is (eql x (first (bound-variables parsed-term))))
        (5am:is (eql '() (binding-keywords xb)))
        (5am:is (typep (binding-context xb) 'local-context))
        (5am:is (eql cc (outer-context (binding-context xb))))
        (let ((arg (argument parsed-term)))
          (5am:is (eql 'unknown-general-application-term (type-of arg)))
          (5am:is (eql x (first (arguments arg)))))))
    (5am:is-true (typep (arguments parsed-term) 'cons))
    (5am:is (= 1 (length (arguments parsed-term)))))
  (5am:signals simple-error
    (parse-into-term-representation (butlast term) cc))
  (5am:signals simple-error
    (parse-into-term-representation (append term '((g ?z))) cc)))

#+5am
(defun test-parse-quantification-2 (term operator cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (5am:is-true (typep parsed-term (term-type-for-operator operator cc)))
    (5am:is (eql operator (operator parsed-term)))
    (destructuring-bind (xb yb) (bindings parsed-term) 
      (5am:is-true (typep xb 'binding))
      (5am:is-true (typep yb 'binding))
      (let ((x (binding-variable xb))
            (y (binding-variable yb)))
        (5am:is-true (typep x 'variable-term))
        (5am:is-true (typep y 'variable-term))
        (5am:is (eql '?x (name x)))
        (5am:is (eql '?y (name y)))
        (5am:is (eql nil (slot-value parsed-term 'bound-variables)))
        (5am:is (= 2 (length (bound-variables parsed-term))))
        ;; Check that we cache the result of bound-variables
        (5am:is (not (eql nil (slot-value parsed-term 'bound-variables))))
        (5am:is (eql x (first (bound-variables parsed-term))))
        (5am:is (eql y (second (bound-variables parsed-term))))
        (5am:is (eql '() (binding-keywords xb)))
        (5am:is (eql '() (binding-keywords yb)))
        (5am:is (typep (binding-context xb) 'local-context))
        (5am:is (eql cc (outer-context (binding-context xb))))
        (5am:is (typep (binding-context yb) 'local-context))
        (5am:is (eql cc (outer-context (binding-context yb))))
        (let ((arg (argument parsed-term)))
          (5am:is (eql 'unknown-general-application-term (type-of arg)))
          (5am:is (eql x (first (arguments arg))))
          (5am:is (eql y (second (arguments arg)))))))
    (5am:is-true (typep (arguments parsed-term) 'cons))
    (5am:is (= 1 (length (arguments parsed-term)))))
  (5am:signals simple-error
    (parse-into-term-representation (butlast term) cc))
  (5am:signals simple-error
    (parse-into-term-representation (append term '((g ?z))) cc)))

#+5am
(5am:test (test-parse-into-term-representation-01
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (let ((foo (parse-into-term-representation '?foo cc)))
      (5am:is-true (typep foo 'variable-term))
      (5am:is (eql '?foo (name foo))))
    (let ((foo (parse-into-term-representation 'foo cc)))
      (5am:is-true (typep foo 'primitive-term))
      (5am:is (eql 'foo (value foo))))
    (let ((num (parse-into-term-representation 123 cc)))
      (5am:is-true (typep num 'number-term))
      (5am:is (eql 123 (value num))))
    (let ((foo (parse-into-term-representation '(foo ?x ?y) cc)))
      (5am:is-true (typep foo 'unknown-general-application-term))
      (5am:is (eql 'foo (operator foo)))
      (5am:is-true (typep (arguments foo) 'cons))
      (5am:is (= 2 (length (arguments foo))))
      (destructuring-bind (x y) (arguments foo)
        (5am:is-true (typep x 'variable-term))
        (5am:is (eql '?x (name x)))
        (5am:is-true (typep y 'variable-term))
        (5am:is (eql '?y (name y)))))))

#+5am
(5am:test (test-parse-into-term-representation-02
           :compile-at :definition-time)
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
    (test-parse-quantification-1 '(foreach ?x (f ?x)) 'foreach cc)
    (test-parse-quantification-1 '(each ?x (f ?x)) 'foreach cc)
    (test-parse-quantification-1 '(forall ?x (f ?x)) 'foreach cc)
    (test-parse-quantification-1 '(exists ?x (f ?x)) 'exists cc)
    (test-parse-quantification-1 '(exist ?x (f ?x)) 'exists cc)
    (test-parse-quantification-2 '(foreach (?x ?y) (f ?x ?y))  'foreach cc)
    (test-parse-quantification-2 '(exists (?x ?y) (f ?x ?y))  'exists cc)
    (test-parse-quantification-2 '(foreach ((?x) (?y)) (f ?x ?y))  'foreach cc)
    (test-parse-quantification-2 '(exists ((?x) (?y)) (f ?x ?y))  'exists cc)))


#+5am
(defun test-parse-empty-program-term (term cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (5am:is-true (typep parsed-term 'empty-program-term))))

#+5am
(5am:test (test-parse-into-term-representation-03
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (test-parse-empty-program-term 'nil cc)
    (test-parse-empty-program-term 'null cc)))

#+5am
(defun test-parse-primitive-action-term (term action-name action-class arity cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (5am:is (typep parsed-term action-class))
    (5am:is (eql action-name (operator parsed-term)))
    (5am:is (consp (arguments parsed-term)))
    (5am:is (= arity (length (arguments parsed-term))))
    (5am:is (typep (first (arguments parsed-term)) 'variable-term))))

(define-primitive-action foo)
(define-primitive-action bar)

(5am:test (test-parse-into-term-representation-04
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (test-parse-primitive-action-term
     '(foo ?x ?y) 'foo 'foo-term 2 cc)))

(5am:test (test-parse-into-term-representation-05
           :compile-at :definition-time)
  (let* ((cc (make-instance 'compilation-unit))
         (parsed-term (parse-into-term-representation '(test (and (f ?x) (g ?x))) cc)))
    (5am:is (typep parsed-term 'test-term))
    (5am:is (eql 'holds? (operator parsed-term)))
    (5am:is (typep (argument parsed-term) 'conjunction-term))
    (destructuring-bind (f g) (arguments (argument parsed-term))
      (5am:is (typep f 'unknown-general-application-term))
      (5am:is (eql 'f (operator f)))
      (5am:is (typep g 'unknown-general-application-term))
      (5am:is (eql 'g (operator g))))))
    

(5am:test (test-parse-into-term-representation-06
           :compile-at :definition-time)
  (let* ((cc (make-instance 'compilation-unit))
         (parsed-term (parse-into-term-representation '(seq (f ?x) (g ?x)) cc)))
    (5am:is (typep parsed-term 'sequence-term))
    (5am:is (eql 'seq (operator parsed-term)))
    (5am:is (= 2 (length (body parsed-term))))
    (destructuring-bind (f g) (body parsed-term)
      (5am:is (typep f 'unknown-general-application-term))
      (5am:is (eql 'f (operator f)))
      (5am:is (typep g 'unknown-general-application-term))
      (5am:is (eql 'g (operator g))))))
    

(5am:test (test-parse-into-term-representation-07
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(seq (foo ?x) (g ?x)) cc)))
      (5am:is (typep parsed-term 'sequence-term))
      (5am:is (eql 'seq (operator parsed-term)))
      (5am:is (= 2 (length (body parsed-term))))
      (destructuring-bind (foo g) (body parsed-term)
        (5am:is (typep foo 'foo-term))
        (5am:is (eql 'foo (operator foo)))
        (5am:is (typep g 'unknown-general-application-term))
        (5am:is (eql 'g (operator g)))))))

(5am:test (test-parse-into-term-representation-08
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (declare-primitive-action 'bar cc)
    (let ((parsed-term (parse-into-term-representation
                        '(one-of (foo north) (foo south) (bar west) (bar east))
                        cc)))
      (5am:is (typep parsed-term 'action-choice-term))
      (5am:is (eql 'one-of (operator parsed-term))))))

(5am:test (test-parse-into-term-representation-09
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation
                        '(pick ?x (foo ?x))
                        cc)))
      (5am:is (typep parsed-term 'argument-choice-term))
      (5am:is (eql 'pick (operator parsed-term))))))

(5am:test (test-parse-into-term-representation-10
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (declare-primitive-action 'bar cc)
    (let ((parsed-term (parse-into-term-representation
                        '(repeat (foo north))
                        cc)))
      (5am:is (typep parsed-term 'iteration-term))
      (5am:is (eql 'repeat (operator parsed-term))))))

(5am:test (test-parse-into-term-representation-11
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (declare-primitive-action 'bar cc)
    (let ((parsed-term (parse-into-term-representation
                        '(if (foo north) (bar east) (bar west))
                        cc)))
      (5am:is (typep parsed-term 'conditional-term))
      (5am:is (eql 'if (operator parsed-term)))
      (5am:is (typep (arg1 parsed-term) 'foo-term))
      (let ((north-arg (first (arguments (arg1 parsed-term)))))
        (5am:is (typep north-arg 'primitive-term))
        (5am:is (eql 'north (value north-arg))))
      (5am:is (typep (arg2 parsed-term) 'bar-term))
      (let ((east-arg (first (arguments (arg2 parsed-term)))))
        (5am:is (typep east-arg 'primitive-term))
        (5am:is (eql 'east (value east-arg))))
      (5am:is (typep (arg3 parsed-term) 'bar-term))
      (let ((west-arg (first (arguments (arg3 parsed-term)))))
        (5am:is (typep west-arg 'primitive-term))
        (5am:is (eql 'west (value west-arg)))))))

(5am:test (test-parse-into-term-representation-12
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation
                        '(while (bar ?x) (foo north))
                        cc)))
      (5am:is (typep parsed-term 'while-loop-term))
      (5am:is (eql 'while (operator parsed-term))))))

(5am:test (test-parse-into-term-representation-13
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(search (foo ?x) (g ?x)) cc)))
      (5am:is (typep parsed-term 'search-term))
      (5am:is (eql 'search (operator parsed-term)))
      (5am:is (= 2 (length (body parsed-term))))
      (destructuring-bind (foo g) (body parsed-term)
        (5am:is (typep foo 'foo-term))
        (5am:is (eql 'foo (operator foo)))
        (5am:is (typep g 'unknown-general-application-term))
        (5am:is (eql 'g (operator g)))))))

(5am:test (test-parse-into-term-representation-14
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(concurrently (foo ?x) (g ?x)) cc)))
      (5am:is (typep parsed-term 'concurrent-term))
      (5am:is (eql 'concurrently (operator parsed-term)))
      (5am:is (= 2 (length (arguments parsed-term))))
      (destructuring-bind (foo g) (arguments parsed-term)
        (5am:is (typep foo 'foo-term))
        (5am:is (eql 'foo (operator foo)))
        (5am:is (typep g 'unknown-general-application-term))
        (5am:is (eql 'g (operator g)))))))

(5am:test (test-parse-into-term-representation-15
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-primitive-action 'foo cc)
    (let ((parsed-term (parse-into-term-representation '(prioritized (foo ?x) (g ?x)) cc)))
      (5am:is (typep parsed-term 'prioritized-concurrent-term))
      (5am:is (eql 'prioritized (operator parsed-term)))
      (5am:is (= 2 (length (arguments parsed-term))))
      (destructuring-bind (foo g) (arguments parsed-term)
        (5am:is (typep foo 'foo-term))
        (5am:is (eql 'foo (operator foo)))
        (5am:is (typep g 'unknown-general-application-term))
        (5am:is (eql 'g (operator g)))))))

(define-functional-fluent robot-position)
(define-relational-fluent holds-item)

(5am:test (test-parse-into-term-representation-16
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-functional-fluent 'holds-item cc)
    (let ((ff (parse-into-term-representation '(holds-item ?robot ?item) cc)))
      (5am:is (typep ff 'holds-item-term))
      (5am:is (typep ff 'known-general-application-term))
      (5am:is (= 2 (length (arguments ff)))))))


(5am:test (test-parse-into-term-representation-17
           :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (declare-relational-fluent 'robot-position cc)
    (let ((rf (parse-into-term-representation '(robot-position ?robot) cc)))
      (5am:is (typep rf 'robot-position-term))
      (5am:is (typep rf 'known-general-application-term))
      (5am:is (= 1 (length (arguments rf)))))))
