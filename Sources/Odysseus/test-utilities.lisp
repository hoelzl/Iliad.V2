;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
(in-suite odysseus-utilities-suite)

(deftest test-name-mixin ()
  (is (eql :<unnamed> (name (make-instance 'name-mixin))))
  (is (eql 'foo (name (make-instance 'name-mixin :name 'foo)))))

(deftest test-required-name-mixin ()
  (signals error (make-instance 'required-name-mixin))
  (is (eql 'foo (name (make-instance 'required-name-mixin :name 'foo)))))

(deftest test-trace-format ()
  (let ((*odysseus-trace-output* (make-string-output-stream))
        (*trace-odysseus* nil))
    (trace-format "abc~Axyz" 123)
    (is (string-equal (get-output-stream-string *odysseus-trace-output*)
                      "")))
  (let ((*odysseus-trace-output* (make-string-output-stream))
        (*trace-odysseus* t))
    (trace-format "abc~Axyz" 123)
    (is (string-equal (get-output-stream-string *odysseus-trace-output*)
                      "abc123xyz"))))

(deftest test-feature-for-lisp-type ()
  (is (member (feature-for-lisp-type) *features*)))

(deftest test-gethash* ()
  (let ((ht (make-hash-table)))
    (is (eql 1 (gethash* :foo ht 1)))
    (is (eql 1 (gethash :foo ht 2)))
    (is (eql 1 (gethash* :foo ht 2)))))

(deftest test-unquote ()
  (is (eql 1 (unquote 1)))
  (is (eql 1 (unquote '1)))
  (is (equalp '1 (unquote ''1)))
  (is (equalp '(1 2 3) (unquote '(1 2 3))))
  (is (equalp '(1 2 3) (unquote ''(1 2 3))))
  (is (equalp ''(1 2 3) (unquote '''(1 2 3)))))

(deftest test-wrap-in-quote ()
  (is (eql 1 (wrap-in-quote 1)))
  (is (eql :foo (wrap-in-quote :foo)))
  (is (equalp ''foo (wrap-in-quote 'foo))))

(deftest test-wrap-in-forall ()
  (is (equalp '(foo x y z) (wrap-in-forall '() '(foo x y z))))
  (is (equalp '(forall (x y z) (foo x y z))
              (wrap-in-forall '(x y z) '(foo x y z)))))

(deftest test-sexpr-equalp ()
  (is (sexpr-equal-p '(foo x y) '(foo x y)))
  (let ((x '#:x) (y '#:y))
    (is (sexpr-equal-p `(foo ,x ,y) `(foo ,x ,y)))
    (is (not (sexpr-equal-p `(foo ,x ,y) `(foo ,y ,x)))))
  (is (sexpr-equal-p '(foo x y) '(foo #:x #:y)))
  (is (sexpr-equal-p '(foo #:x #:y) '(foo #:x #:y)))
  (let ((x '#:bar))
    (is (sexpr-equal-p '(foo x x) `(foo ,x ,x)))
    (is (sexpr-equal-p '(foo y y) `(foo ,x ,x)))
    (is (not (sexpr-equal-p '(foo x y) `(foo ,x ,x))))
    (is (sexpr-equal-p '(foo x #:bar) `(foo ,x ,x)))))

(deftest test-make-uuid ()
  (let ((uuid (make-uuid)))
    (is (= 36 (length uuid)))
    (is (eql #\- (aref uuid 8)))
    (is (eql #\- (aref uuid 13)))
    (is (eql #\- (aref uuid 18)))
    (is (eql #\- (aref uuid 23)))
    (is (eql #\4 (aref uuid 14)))
    ;; The standard allows this to be 8, 9, A or B, but our algorithm always
    ;; generates A.
    (is (eql #\A (aref uuid 19)))))

(deftest test-make-uuid-symbol ()
  (let* ((random-state (make-random-state))
         (uuid (make-uuid))
         (*random-state* random-state)
         (uuid-key (make-uuid-symbol)))
    (is (string= uuid (symbol-name uuid-key)))))

(deftest test-uncons ()
  (is (= 1 (uncons 1)))
  (is (= 1 (uncons '(1)))))

(deftest test-process-argument-arglist ()
  (is (equalp '(x y z foo bar)
              (process-argument-arglist
               '(x (y t) (z symbol) &optional foo (bar 1)))))
  (is (equalp '(x y z :foo foo :bar bar)
              (process-argument-arglist
               '(x (y t) (z symbol) &key foo (bar 1))))))

(deftest test-boolean3-deftype ()
  (is (typep t 'boolean3))
  (is (typep :unknown 'boolean3))
  (is (typep nil 'boolean3))
  (is (not (typep 5 'boolean3)))
  (is (not (typep :foo 'boolean3))))

(deftest test-and3 ()
  (is (and3))
  (is (eql nil (and3 nil)))
  (is (eql :unknown (and3 :unknown)))
  (is (eql t (and3 t)))
  (is (eql :unknown (and3 t t :unknown t t))))

(deftest test-or3 ()
  (is (not (or3)))  (is (eql nil (or3 nil)))
  (is (eql :unknown (or3 :unknown)))
  (is (eql t (or3 t)))
  (is (eql :unknown (or3 nil nil :unknown nil nil))))
