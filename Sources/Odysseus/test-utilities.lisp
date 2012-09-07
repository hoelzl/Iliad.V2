;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-tests)
#+5am
(5am:in-suite odysseus-utilities-suite)

#+5am
(5am:test (test-boolean3-deftype :compile-at :definition-time)
  (5am:is-true (typep t 'boolean3))
  (5am:is-true (typep :unknown 'boolean3))
  (5am:is-true (typep nil 'boolean3))
  (5am:is-false (typep 5 'boolean3))
  (5am:is-false (typep :foo 'boolean3)))

#+5am
(5am:test test-(and3 :compile-at :definition-time)
  (5am:is-true (and3))
  (5am:is (eql nil (and3 nil)))
  (5am:is (eql :unknown (and3 :unknown)))
  (5am:is (eql t (and3 t)))
  (5am:is (eql :unknown (and3 t t :unknown t t))))

#+5am
(5am:test (test-or3 :compile-at :definition-time)
  (5am:is-false (or3))
  (5am:is (eql nil (or3 nil)))
  (5am:is (eql :unknown (or3 :unknown)))
  (5am:is (eql t (or3 t)))
  (5am:is (eql :unknown (or3 nil nil :unknown nil nil))))
