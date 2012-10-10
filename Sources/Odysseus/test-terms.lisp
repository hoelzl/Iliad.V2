;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-syntax-suite)

(deftest test-known-operators-for-compilation-unit ()
  (let* ((cu (make-instance 'compilation-unit))
         (ops (known-operators cu)))
    (is (typep ops 'hash-table))
    (doplist (op type odysseus::*logical-operators*)
      (is (eql type (gethash op ops nil))))))

(deftest test-make-variable ()
  (let ((cc (make-instance 'compilation-context))
	(var (make-variable-term 'foo 'bar nil :intern nil)))
    (is (eql 'foo (name var)))
    (is (eql 'bar (declared-sort var cc)))
    (is (symbolp (unique-name var)))
    ;; Check that the unique name is in fact interned.
    (is (eql (unique-name var) (unique-name var)))
    (is (eql nil (symbol-package (unique-name var))))
    (let ((var2 (make-variable-term 'foo 'bar nil :intern nil)))
      (is (not (eql (unique-name var) (unique-name var2)))))))


(deftest test-unary-term ()
  (let ((t1 (make-instance 'unary-term :argument 1 :context nil))
        (t2 (make-instance 'unary-term :arguments '(2) :context nil)))
    (is (= (argument t1) 1))
    (is (= (argument t2) 2))
    (is (equalp (arguments t1) '(1)))
    (is (equalp (arguments t2) '(2)))
    (setf (arguments t1) '(11)
          (argument t2) 22)
    (is (= (argument t1) 11))
    (is (= (argument t2) 22))
    (is (equalp (arguments t1) '(11)))
    (is (equalp (arguments t2) '(22))))
  (signals simple-error
    (make-instance 'unary-term :argument 1 :arguments '(1) :context nil))
  (signals simple-error
    (make-instance 'unary-term :arguments '(1 2) :context nil)))

(deftest test-binary-term ()
  (let ((t1 (make-instance 'binary-term :lhs 1 :rhs 2 :context nil))
        (t2 (make-instance 'binary-term :arguments '(3 4) :context nil)))
    (is (= (lhs t1) 1))
    (is (= (rhs t1) 2))
    (is (= (lhs t2) 3))
    (is (= (rhs t2) 4))
    (is (equalp (arguments t1) '(1 2)))
    (is (equalp (arguments t2) '(3 4)))
    (setf (arguments t1) '(11 22)
          (lhs t2) 33 (rhs t2) 44)
    (is (= (lhs t1) 11))
    (is (= (rhs t1) 22))
    (is (= (lhs t2) 33))
    (is (= (rhs t2) 44))
    (is (equalp (arguments t1) '(11 22)))
    (is (equalp (arguments t2) '(33 44))))
  (signals simple-error
    (make-instance 'binary-term :lhs 1 :arguments '(1 2) :context nil))
  (signals simple-error
    (make-instance 'binary-term :rhs 1 :arguments '(1 2) :context nil))
  (signals simple-error
    (make-instance 'binary-term :arguments '(1) :context nil))
  (signals simple-error
    (make-instance 'binary-term :arguments '(1 2 3) :context nil)))

(deftest test-ternary-term ()
  (let ((t1 (make-instance 'ternary-term :arg1 1 :arg2 2 :arg3 3 :context nil))
        (t2 (make-instance 'ternary-term :arguments '(4 5 6) :context nil)))
    (is (= 1 (arg1 t1)))
    (is (= 2 (arg2 t1)))
    (is (= 3 (arg3 t1)))
    (is (= 4 (arg1 t2)))
    (is (= 5 (arg2 t2)))
    (is (= 6 (arg3 t2)))
    (is (equalp (arguments t1) '(1 2 3)))
    (is (equalp (arguments t2) '(4 5 6)))
    (setf (arguments t1) '(11 22 33)
          (arg1 t2) 44 (arg2 t2) 55 (arg3 t2) 66)
    (is (= 11 (arg1 t1)))
    (is (= 22 (arg2 t1)))
    (is (= 33 (arg3 t1)))
    (is (= 44 (arg1 t2)))
    (is (= 55 (arg2 t2)))
    (is (= 66 (arg3 t2)))
    (is (equalp (arguments t1) '(11 22 33)))
    (is (equalp (arguments t2) '(44 55 66))))
  (signals simple-error
    (make-instance 'ternary-term :arg1 1 :arguments '(1 2 3) :context nil))
  (signals simple-error
    (make-instance 'ternary-term :arg2 1 :arguments '(1 2 3) :context nil))
  (signals simple-error
    (make-instance 'ternary-term :arg3 1 :arguments '(1 2 3) :context nil))
  (signals simple-error
    (make-instance 'ternary-term :arguments '(1 2) :context nil))
  (signals simple-error
    (make-instance 'ternary-term :arguments '(1 2 3 4) :context nil)))
