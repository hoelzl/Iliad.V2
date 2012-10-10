;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-substitution-suite)

(deftest test-apply-empty-substitution ()
  (with-terms
    (is (eql 'f (operator call-term)))
    (is (eql x (first (arguments call-term))))
    (is (eql y (second (arguments call-term))))
    (let ((result (apply-substitution
                   call-term 
                   (make-instance 'empty-substitution))))
      (is (eql 'f (operator call-term)))
      (is (eql x (first (arguments call-term))))
      (is (eql y (second (arguments call-term))))
      (is (eql 'f (operator result)))
      (is (eql x (first (arguments result))))
      (is (eql y (second (arguments result)))))))

(deftest test-apply-non-empty-substitution ()
  (with-terms
    (is (eql 'f (operator call-term)))
    (is (eql x (first (arguments call-term))))
    (is (eql y (second (arguments call-term))))
    (let ((result (apply-substitution
                   call-term 
                   (make-instance 'non-empty-substitution
                     :old-terms (list x z)
                     :new-terms (list var1 var2)))))
      (is (eql 'f (operator call-term)))
      (is (eql x (first (arguments call-term))))
      (is (eql y (second (arguments call-term))))
      (is (eql 'f (operator result)))
      (is (eql var1 (first (arguments result))))
      (is (eql y (second (arguments result)))))))

