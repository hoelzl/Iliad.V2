;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-continuation-suite)

(deftest test-apply-substitution-for-continuations ()
  (with-terms
    (let* ((cont (make-instance 'continuation
                   :term call-term
                   :situation situation))
           (new-cont (apply-substitution cont subst)))
      (is (eql 'f (operator (term new-cont))))
      (is (equalp (list var1 y) (arguments (term new-cont))))
      (is (eql (previous-situation situation)
               (previous-situation (situation new-cont)))))))
      

(deftest test-make-continuation-generators ()
  (with-terms
    (is (eql (the-empty-continuation-generator)
             (make-instance 'continuation-generator)))
    (is (typep (make-instance 'continuation-generator)
               'empty-continuation-generator))
    (is (typep (make-instance 'continuation-generator
                 :continuations (list call-term))
               'list-continuation-generator))
    (is (= 1 (length (continuations (make-instance 'continuation-generator
                                      :continuations (list call-term))))))))


(deftest test-empty-continuation-generator ()
  (is (null (peek-next-continuation (the-empty-continuation-generator))))
  (is (null (pop-next-continuation (the-empty-continuation-generator))))
  (is (null (continuations (the-empty-continuation-generator)))))


(deftest test-non-empty-continuation-generators ()
  (with-terms
    (let ((cg1 (make-instance 'continuation-generator
                 :continuations (list call-term))))
      (is (eql call-term (peek-next-continuation cg1)))
      (is (eql call-term (pop-next-continuation cg1)))
      (is (null (peek-next-continuation cg1)))
      (is (null (pop-next-continuation cg1))))
    (let ((cg3 (make-instance 'continuation-generator
                 :continuations (list call-term-1 call-term-2 call-term-3))))
      (is (eql call-term-1 (peek-next-continuation cg3)))
      (is (eql call-term-1 (pop-next-continuation cg3)))
      (is (eql call-term-2 (peek-next-continuation cg3)))
      (is (eql call-term-2 (pop-next-continuation cg3)))
      (is (eql call-term-3 (peek-next-continuation cg3)))
      (is (eql call-term-3 (pop-next-continuation cg3)))
      (is (null (peek-next-continuation cg3)))
      (is (null (pop-next-continuation cg3))))))


(deftest test-append-continuation-generators ()
  (with-terms
    (let ((cg0 (make-instance 'continuation-generator
                 :continuations '()))
          (cg1 (make-instance 'continuation-generator
                 :continuations (list call-term)))
          (cg2 (make-instance 'continuation-generator
                 :continuations (list call-term-1 call-term-2)))
          (cg3 (make-instance 'continuation-generator
                 :continuations (list call-term-1 call-term-2 call-term-3))))
      (is (eql (the-empty-continuation-generator)
               (append-continuations cg0 cg0)))
      (is (eql cg1 (append-continuations cg0 cg1)))
      (is (eql cg1 (append-continuations cg1 cg0)))
      (is (equalp (list call-term-1 call-term-2 call-term-1 call-term-2 call-term-3)
                  (continuations (append-continuations cg2 cg3)))))))


(deftest test-copy-continuation-generator ()
  (let* ((cgen (make-instance 'continuation-generator
                 :continuations '(1 2 3 4)))
         (cgen-copy (copy-continuation-generator cgen)))
    (is (typep cgen-copy 'list-continuation-generator))
    (is (eql (continuations cgen) (continuations cgen-copy)))
    (is (not (eql cgen cgen-copy))))
  (let* ((cgen (make-instance 'empty-continuation-generator))
         (cgen-copy (copy-continuation-generator cgen)))
    (is (typep cgen-copy 'empty-continuation-generator))
    (is (not (eql cgen cgen-copy)))))


(deftest test-apply-substitution-for-continuation-generators ()
  (with-terms
    (let* ((cg (make-instance 'continuation-generator
                 :continuations (list call-term-0 call-term-1 call-term-2)))
           (new-cg (apply-substitution cg subst))
           (new-conts (continuations new-cg)))
      (is (eql cg (apply-substitution cg (the-empty-substitution))))
      (is (= 3 (length new-conts)))
      (is (eql 'f0 (operator (first new-conts))))
      (is (null (arguments (first new-conts))))
      (is (eql 'f1 (operator (second new-conts))))
      (let ((args (arguments (second new-conts))))
        (is (= 1 (length args)))
        (is (eql var1 (first args))))
      (is (eql 'f2 (operator (third new-conts))))
      (let ((args (arguments (third new-conts))))
        (is (= 2 (length args)))
        (is (eql var1 (first args)))
        (is (eql y (second args)))))))
        

(deftest test-extend-continuation-01 ()
  (with-terms
    (let* ((old-cont (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
           (new-cont (extend-continuation old-cont
                                          call-term-2
                                          (the-empty-substitution)
                                          'body-term)))
      (is (typep new-cont 'continuation))
      (is (typep (term new-cont) 'body-term))
      (let ((body (body (term new-cont))))
        (is (= 2 (length body)))
        (is (eql call-term-1 (first body)))
        (is (eql call-term-2 (second body))))
      (is (eql situation (situation new-cont)))
      (is (eql call-term (deferred-proofs new-cont))))))

(deftest test-extend-continuation-02 ()
  (with-terms
    (let* ((old-cont (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
           (new-cont (extend-continuation old-cont
                                          call-term-2
                                          subst
                                          'body-term)))
      (is (typep new-cont 'continuation))
      (is (typep (term new-cont) 'body-term))
      (let* ((body (body (term new-cont)))
             (ct1 (first body))
             (ct2 (second body)))
        (is (= 2 (length body)))
        (is (eql 'f1 (operator ct1)))
        (is (= 1 (length (arguments ct1))))
        (is (eql var1 (first (arguments ct1))))
        (is (eql 'f2 (operator ct2)))
        (is (= 2 (length (arguments ct2))))
        (is (eql var1 (first (arguments ct2))))
        (is (eql y (second (arguments ct2)))))
      (is (eql situation (situation new-cont)))
      (is (eql call-term (deferred-proofs new-cont))))))

(deftest test-extend-continuation-03 ()
  (with-terms
    (let* ((old-cont (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
           (new-cont (extend-continuation old-cont
                                          '()
                                          (the-empty-substitution)
                                          'body-term)))
      (is (eql old-cont new-cont)))))

(deftest test-extend-continuation-04 ()
  (with-terms
    (let* ((old-cont (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
           (new-cont (extend-continuation old-cont
                                          '()
                                          subst
                                          'body-term)))
      (is (typep new-cont 'continuation))
      (let ((term (term new-cont)))
        (is (typep term 'unknown-general-application-term))
        (is (eql 'f1 (operator term)))
        (is (= 1 (length (arguments term))))
        (is (eql var1 (first (arguments term)))))
      (is (eql situation (situation new-cont)))
      (is (eql call-term (deferred-proofs new-cont))))))

(deftest test-extend-continuation-05 ()
  (with-terms
    (let* ((old-cont (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
           (new-cont (extend-continuation old-cont
                                          (list call-term-2 call-term-3)
                                          (the-empty-substitution)
                                          'body-term)))
      (is (typep new-cont 'continuation))
      (is (typep (term new-cont) 'body-term))
      (let ((body (body (term new-cont))))
        (is (= 3 (length body)))
        (is (eql call-term-1 (first body)))
        (is (eql call-term-2 (second body)))
        (is (eql call-term-3 (third body))))
      (is (eql situation (situation new-cont)))
      (is (eql call-term (deferred-proofs new-cont))))))

(deftest test-extend-continuation-06 ()
  (with-terms
    (let* ((old-cont (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
           (new-cont (extend-continuation old-cont
                                          (list call-term-2 call-term-3)
                                          subst
                                          'body-term)))
      (is (typep new-cont 'continuation))
      (is (typep (term new-cont) 'body-term))
      (let* ((body (body (term new-cont)))
             (ct1 (first body))
             (ct2 (second body))
             (ct3 (third body)))
        (is (= 3 (length body)))
        (is (eql 'f1 (operator ct1)))
        (is (= 1 (length (arguments ct1))))
        (is (eql var1 (first (arguments ct1))))
        (is (eql 'f2 (operator ct2)))
        (is (= 2 (length (arguments ct2))))
        (is (eql var1 (first (arguments ct2))))
        (is (eql y (second (arguments ct2))))
        (is (eql 'f3 (operator ct3)))
        (is (= 3 (length (arguments ct3))))
        (is (eql var1 (first (arguments ct3))))
        (is (eql y (second (arguments ct3))))
        (is (eql var2 (third (arguments ct3)))))
      (is (eql situation (situation new-cont)))
      (is (eql call-term (deferred-proofs new-cont))))))

(deftest test-extend-continuations ()
  (with-terms
    (flet ((test-extend-continuation-results (nc1 nc2)
             (is (typep nc1 'continuation))
             (is (typep (term nc1) 'body-term))
             (let ((body (body (term nc1))))
               (is (= 2 (length body)))
               (let ((term1 (first body))
                     (term2 (second body)))
                 (is (eql 'f1 (operator term1)))
                 (is (= 1 (length (arguments term1))))
                 (is (eql x (first (arguments term1))))
                 (is (eql 'f3 (operator term2)))
                 (is (= 3 (length (arguments term2))))
                 (is (eql x (first (arguments term2))))
                 (is (eql y (second (arguments term2))))
                 (is (eql z (third (arguments term2))))))
             (is (typep nc2 'continuation))
             (is (typep (term nc2) 'body-term))
             (let ((body (body (term nc2))))
               (is (= 2 (length body)))
               (let ((term1 (first body))
                     (term2 (second body)))
                 (is (eql 'f2 (operator term1)))
                 (is (= 2 (length (arguments term1))))
                 (is (eql x (first (arguments term1))))
                 (is (eql y (second (arguments term1))))
                 (is (eql 'f3 (operator term2)))
                 (is (= 3 (length (arguments term2))))
                 (is (eql x (first (arguments term2))))
                 (is (eql y (second (arguments term2))))
                 (is (eql z (third (arguments term2))))))))

      (let* ((cont-1 (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
             (cont-2 (make-instance 'continuation
                       :term call-term-2
                       :situation situation
                       :deferred-proofs '()))
             (result (extend-continuations
                      (list cont-1 cont-2)
                      call-term-3
                      :substitution (the-empty-substitution))))
        (is (typep result 'cons))
        (is (= 2 (length result)))
        (let ((nc1 (first result))
              (nc2 (second result)))
          (test-extend-continuation-results nc1 nc2)))

      (let* ((cont-1 (make-instance 'continuation
                       :term call-term-1
                       :situation situation
                       :deferred-proofs call-term))
             (cont-2 (make-instance 'continuation
                       :term call-term-2
                       :situation situation
                       :deferred-proofs '()))
             (cg (make-instance 'continuation-generator
                   :continuations (list cont-1 cont-2)))
             (result (extend-continuations
                      cg
                      call-term-3
                      :substitution (the-empty-substitution))))
        (is (typep result 'continuation-generator))
        (let ((conts (continuations result)))
          (is (typep conts 'cons))
          (is (= 2 (length conts)))
          (let ((nc1 (first conts))
                (nc2 (second conts)))
            (test-extend-continuation-results nc1 nc2)))))))
