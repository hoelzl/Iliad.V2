;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-interpreter-suite)

(deftest test-interpreter-class ()
  (let* ((interpreter (make-instance 'interpreter))
         (empty-term (make-instance 'empty-program-term
                       :context (context interpreter)))
         (primitive-action-term (make-instance 'primitive-action-term
                                  :context (context interpreter))))
    (is (symbolp (interpreter-uuid interpreter)))
    (is (null (superordinate-interpreter interpreter)))
    (signals cannot-interpret-term
      (interpret-1 interpreter
                   empty-term
                   (the-initial-situation)))
    (signals cannot-interpret-term
      (interpret-1 interpreter
                   'null
                   (the-initial-situation)))
    (signals cannot-interpret-term
      (interpret-1 interpreter
                   '(null)
                   (the-initial-situation)))
    (signals can-execute-p-not-defined
      (can-execute-p interpreter
                     primitive-action-term
                     (the-initial-situation)))
    (signals non-executing-interpreter-error
      (onlinep interpreter))
    #+(or)
    (signals non-executing-interpreter-error
      (run-interpreter-loop interpreter empty-term (the-initial-situation)))
    (signals cannot-execute-primitive-action
      (execute-primitive-action interpreter primitive-action-term))))

(deftest test-make-continuation-01 ()
  (let* ((interpreter (make-instance 'interpreter))
         (empty-term (make-instance 'empty-program-term
                       :context (context interpreter)))
         (proofs '(:some-proof))
         (cont (make-continuation interpreter
                                  empty-term
                                  (the-initial-situation)
                                  proofs)))
    (is (typep cont 'continuation))
    (is (eql (term cont) empty-term))
    (is (eql (situation cont) (the-initial-situation)))
    (is (eql (deferred-proofs cont) proofs))))

(deftest test-make-continuation-generator-01 ()
  (let* ((interpreter (make-instance 'interpreter))
         (empty-term (make-instance 'empty-program-term
                       :context (context interpreter)))
         (proofs '(:some-proof)))
    (signals warning
      (make-continuation-generator interpreter empty-term (the-initial-situation)
                                   proofs :no-reason-at-all '()))))

(deftest test-interpreter-forward-declarations ()
  (let ((interpreter (make-instance 'interpreter)))
    (setf (primitive-action-definition 'foo interpreter)
          (make-instance 'primitive-action-definition
            :operator 'foo
            :signature '(t t)
            :class 'primitive-action-term
            :context (context interpreter)))
    (is (eql (primitive-action-definition 'foo interpreter)
             (primitive-action-definition 'foo (context interpreter))))))

(deftest test-basic-interpreter-class ()
  (let* ((interp-1 (make-instance 'basic-interpreter))
         (interp-2 (make-instance 'basic-interpreter
                     :superordinate-interpreter interp-1)))
    (is (null (superordinate-interpreter interp-1)))
    (is (typep (context interp-1) 'top-level-context))
    (is (eql interp-1 (superordinate-interpreter interp-2)))
    (is (eql (context interp-2) (context interp-1)))))

(deftest test-prove-for-basic-interpreter ()
  (let* ((osnark::*use-resolution-only* t)
         (*trace-odysseus* nil)
         (interp (make-instance 'basic-interpreter))
         (theory (parse-into-term-representation
                  '(assert '(forall (?x) (p ?x)))
                  (context interp)))
         (formula (parse-into-term-representation
                   '(p ?x)
                   (context interp))))
    (vector-push-extend theory (declarations (context interp)))
    (multiple-value-bind (result reason free-variables answer)
        (prove interp formula)
      (is (eql t result))
      (is (eql :proof-found reason))
      (is (consp free-variables))
      (is (gethash 'x (variable-hash-table (context interp)))
          (first free-variables))
      (is (consp answer))
      (is (= 2 (length answer)))
      (is (symbolp (second answer))))))

(deftest test-can-execute-for-basic-interpreter-01 ()
  (let* ((osnark::*use-resolution-only* t)
         (*trace-odysseus* nil)
         (interp (make-instance 'basic-interpreter))
         (theory (parse-into-term-representation
                  '(assert '(forall (?x) (p ?x)))
                  (context interp)))
         (action-name 'action-for-test-can-execute-for-basic-interpreter-01)
         (action-class-name 'action-for-test-can-execute-for-basic-interpreter-01-term))
    (vector-push-extend theory (declarations (context interp)))
    (define-primitive-action action-name '(t t)
      :class-name action-class-name
      :force-redefinition t)
    (declare-primitive-action action-name (context interp))
    (let ((action (make-instance action-class-name
                    :context (context interp))))
      (multiple-value-bind (result reason free-variables answer)
          (can-execute-p interp action (the-initial-situation))
        (is (eql t result))
        (is (eql :no-precondition reason))
        (is (eql nil free-variables))
        (is (eql nil answer))))))

(deftest test-can-execute-for-basic-interpreter-02 ()
  (let* ((osnark::*use-resolution-only* t)
         (osnark::*print-snark-output* nil)
         (*trace-odysseus* nil)
         (interp (make-instance 'basic-interpreter))
         (action-name 'action-for-test-can-execute-for-basic-interpreter-02)
         (action-class-name 'action-for-test-can-execute-for-basic-interpreter-02-term)
         (theory (parse-into-term-representation
                  `(assert '(forall (?x ?y ?s)
                             (poss (,action-name ?x ?y) ?s)))
                  (context interp))))
    (vector-push-extend theory (declarations (context interp)))
    (define-primitive-action action-name '(t t)
      :class-name action-class-name
      :precondition `(poss (,action-name ?x ?y) s)
      :force-redefinition t)
    (declare-primitive-action action-name (context interp))
    (declare-operator-sort action-name '(t t) (context interp))
    (let ((action-term (make-instance action-class-name
                         :arguments '(a b)
                         :context (context interp))))
      (is (typep (primitive-action-definition action-name (context interp))
                 'primitive-action-definition))
      (is (equalp '(t t) (declared-sort action-term (context interp))))
      (multiple-value-bind (result reason free-variables answer)
          (can-execute-p interp action-term (the-initial-situation))
        (is (eql t result))
        (is (eql :proof-found reason))
        (is (eql nil free-variables))
        (is (eql nil answer))))))

(deftest test-can-execute-for-basic-interpreter-03 ()
  (with-interpreter
    (is (typep (primitive-action-definition action-name (context interp))
               'primitive-action-definition))
    (is (equalp '(t t) (declared-sort action-term (context interp))))
    (multiple-value-bind (result reason free-variables answer)
        (can-execute-p interp action-term (the-initial-situation))
      (is (eql t result))
      (is (eql :proof-found reason))
      (is (equalp (list x y) free-variables))
      (is (= 3 (length answer))))))

    
