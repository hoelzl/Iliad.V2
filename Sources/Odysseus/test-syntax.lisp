;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-tests)

#+5am
(5am:in-suite odysseus-syntax-suite)

#+5am
(5am:test (test-known-operators-for-compilation-unit
           :compile-at :definition-time)
  (let* ((cu (make-instance 'compilation-unit))
         (ops (known-operators cu)))
    (5am:is-true (typep ops 'hash-table))
    (doplist (op type syntax::*logical-operators*)
      (5am:is (eql type (gethash op ops nil))))))

#+5am
(5am:test (test-global-lookup-for-variable
           :compile-at :definition-time)
  (let* ((cu (make-instance 'compilation-unit))
         (vars (syntax::variable-hash-table cu)))
    (5am:is (= (hash-table-count vars) 0))
    (5am:is (eql (lookup-variable 'foo cu nil) nil))
    (5am:is (= (hash-table-count vars) 0))
    (let ((foo (lookup-variable 'foo cu t))
          (bar (make-instance 'variable-term
                              :name 'bar :intern nil
                              :context cu)))
      (5am:is (= (hash-table-count vars) 1))
      (5am:is (eql (gethash 'foo vars) foo))
      (5am:is (eql (lookup-variable 'foo cu) foo))
      (setf (lookup-variable 'bar cu) bar)
      (5am:is (= (hash-table-count vars) 2))
      (5am:is (eql (gethash 'bar vars) bar))
      (5am:is (eql (lookup-variable 'bar cu) bar))
      (5am:is (eql (gethash 'foo vars) foo))
      (5am:is (eql (lookup-variable 'foo cu) foo)))))

#+5am
(5am:test (test-local-lookup-for-variable
           :compile-at :definition-time)
  (let* ((cu (make-instance 'compilation-unit))
         (vars (syntax::variable-hash-table cu))
         (lc (make-instance 'local-context :outer-context cu)))
    (5am:is (eql (local-variables lc) '()))
    (5am:is (eql (lookup-variable 'foo lc nil) nil))
    (5am:is (eql (local-variables lc) '()))
    (let ((foo (lookup-variable 'foo lc t))
          (bar (make-instance 'variable-term
                              :name 'bar :intern nil
                              :context lc))
          (baz (make-instance 'variable-term
                              :name 'baz :intern nil
                              :context lc)))
      (5am:is (equalp (local-variables lc) (list (cons 'foo foo))))
      (5am:is (= (hash-table-count vars) 0))
      (5am:is (eql foo (lookup-variable 'foo lc nil)))
      (5am:is (eql nil (lookup-variable 'bar lc nil)))
      (5am:is (eql nil (lookup-variable 'baz lc nil)))

      (setf (lookup-variable 'bar lc) bar)
      (5am:is (= (hash-table-count vars) 0))
      (5am:is (equalp (assoc 'bar (local-variables lc)) (cons 'bar bar)))
      (5am:is (eql foo (lookup-variable 'foo lc nil)))
      (5am:is (eql bar (lookup-variable 'bar lc nil)))
      (5am:is (eql nil (lookup-variable 'baz lc nil)))

      (setf (lookup-variable 'baz cu) baz)
      (5am:is (= (hash-table-count vars) 1))
      (5am:is (eql foo (lookup-variable 'foo lc nil)))
      (5am:is (eql bar (lookup-variable 'bar lc nil)))
      (5am:is (eql baz (lookup-variable 'baz lc nil)))
      (5am:is (eql nil (lookup-variable 'foo cu nil)))
      (5am:is (eql nil (lookup-variable 'bar cu nil)))
      (5am:is (eql baz (lookup-variable 'baz cu nil))))))



#+5am
(5am:test (test-variable-interning-in-compilation-unit
           :compile-at :definition-time)
  (let ((cu (make-instance 'compilation-unit)))
    (5am:is (eq (make-instance 'variable-term :name 'f :context cu)
		(make-instance 'variable-term :name 'f :context cu)))
    (5am:is (eq (make-instance 'variable-term :name 'g :context cu)
		(make-instance 'variable-term :name 'g :context cu)))
    (5am:is (not (eq (make-instance 'variable-term :name 'f :context cu)
		     (make-instance 'variable-term :name 'g :context cu))))))

#+5am
(5am:test (test-global-lookup-for-number
           :compile-at :definition-time)
  (let* ((cu (make-instance 'compilation-unit))
         (nums (syntax::number-hash-table cu)))
    (5am:is (= (hash-table-count nums) 0))
    (5am:is (eql (lookup-number 1 cu nil) nil))
    (5am:is (= (hash-table-count nums) 0))
    (let ((foo (lookup-number 1 cu t))
          (bar (make-instance
                'number-term :value 2 :intern nil :context cu)))
      (5am:is (= (hash-table-count nums) 1))
      (5am:is (eql (gethash 1 nums) foo))
      (5am:is (eql (lookup-number 1 cu) foo))
      (setf (lookup-number 2 cu) bar)
      (5am:is (= (hash-table-count nums) 2))
      (5am:is (eql (gethash 2 nums) bar))
      (5am:is (eql (lookup-number 2 cu) bar))
      (5am:is (eql (gethash 1 nums) foo))
      (5am:is (eql (lookup-number 1 cu) foo)))))

#+5am
(5am:test (test-local-lookup-for-number
           :compile-at :definition-time)
  (let* ((cu (make-instance 'compilation-unit))
         (lc (make-instance 'local-context :outer-context cu)))
    (5am:is (eql (lookup-number 123 lc nil) nil))
    (let ((n234 (lookup-number 234 lc t))
          (n345 (make-instance
                 'number-term :value 345 :intern nil :context lc)))
      (5am:is (eql n234 (lookup-number 234 lc nil)))
      (5am:is (eql nil  (lookup-number 345 lc nil)))

      (setf (lookup-number 345 cu) n345)
      (5am:is (eql n234 (lookup-number 234 lc nil)))
      (5am:is (eql n345 (lookup-number 345 lc nil)))
      (5am:is (eql nil  (lookup-number 456 lc nil)))
      (5am:is (eql n234 (lookup-number 234 cu nil)))
      (5am:is (eql n345 (lookup-number 345 cu nil)))
      (5am:is (eql nil  (lookup-number 456 cu nil))))))

#+5am
(5am:test (test-number-interning-in-compilation-unit
           :compile-at :definition-time)
  (let ((cu (make-instance 'compilation-unit)))
    (5am:is (not (eq (make-instance 'number-term :value 0 :context cu :intern nil)
		     (make-instance 'number-term :value 0 :context cu :intern nil))))
    (5am:is (eq (make-instance 'number-term :value 0 :context cu)
		(make-instance 'number-term :value 0 :context cu)))
    (5am:is (eq (make-instance 'number-term :value 1 :context cu)
		(make-instance 'number-term :value 1 :context cu)))
    (5am:is (not (eq (make-instance 'number-term :value 0 :context cu)
		     (make-instance 'number-term :value 1 :context cu))))))

#+5am
(5am:test (test-global-lookup-for-functor
           :compile-at :definition-time)
  (let* ((cu (make-instance 'compilation-unit))
         (funs (syntax::functor-hash-table cu)))
    (5am:is (= (hash-table-count funs) 0))
    (5am:is (eql (lookup-functor 'foo 2 cu nil) nil))
    (let ((foo/2 (lookup-functor 'foo 2 cu t))
          (foo/3 (lookup-functor 'foo 3 cu t))
          (bar (make-instance
                'functor-term :name 'bar :arity 3 :context cu :intern nil)))
      (5am:is (= (hash-table-count funs) 1))
      (5am:is (not (eql foo/2 foo/3)))
      (5am:is (eql (lookup-functor 'foo 2 cu) foo/2))
      (setf (lookup-functor 'bar 3 cu) bar)
      (5am:is (= (hash-table-count funs) 2))
      (5am:is (eql (lookup-functor 'bar 3 cu) bar))
      (5am:is (eql (lookup-functor 'foo 2 cu) foo/2)))))

#+5am
(5am:test (test-local-lookup-for-functor
           :compile-at :definition-time)
  (let* ((cu (make-instance 'compilation-unit))
         (funs (syntax::functor-hash-table cu))
         (lc (make-instance 'local-context :outer-context cu)))
    (5am:is (= (hash-table-count funs) 0))
    (5am:is (eql (lookup-functor 'foo 2 lc nil) nil))
    (let ((foo/2 (lookup-functor 'foo 2 lc t))
          (foo/3 (lookup-functor 'foo 3 lc t))
          (bar (make-instance
                'functor-term :name 'bar :arity 3 :context lc :intern nil)))
      (5am:is (= (hash-table-count funs) 1))
      (5am:is (not (eql foo/2 foo/3)))
      (5am:is (eql (lookup-functor 'foo 2 lc) foo/2))
      (setf (lookup-functor 'bar 3 lc) bar)
      (5am:is (= (hash-table-count funs) 2))
      (5am:is (eql (lookup-functor 'bar 3 lc) bar))
      (5am:is (eql (lookup-functor 'foo 2 lc) foo/2)))))

#+5am
(5am:test (test-functor-interning-in-compilation-unit
           :compile-at :definition-time)
  (let ((cu (make-instance 'compilation-unit)))
    (5am:is (not (eq (make-instance 'functor-term :name 'f :arity 1 :context cu :intern nil)
		     (make-instance 'functor-term :name 'f :arity 1 :context cu :intern nil))))
    (5am:is (eq (make-instance 'functor-term :name 'f :arity 1 :context cu)
		(make-instance 'functor-term :name 'f :arity 1 :context cu)))
    (5am:is (eq (make-instance 'functor-term :name 'f :arity 1 :context cu)
		(make-instance 'functor-term :name 'f :arity 1 :context cu)))
    (5am:is (not (eq (make-instance 'functor-term :name 'f :arity 0 :context cu)
		     (make-instance 'functor-term :name 'f :arity 1 :context cu))))))


#+5am
(5am:test (test-unary-term :compile-at :definition-time)
  (let ((t1 (make-instance 'unary-term :argument 1 :context nil))
        (t2 (make-instance 'unary-term :arguments '(2) :context nil)))
    (5am:is (= (argument t1) 1))
    (5am:is (= (argument t2) 2))
    (5am:is (equalp (arguments t1) '(1)))
    (5am:is (equalp (arguments t2) '(2)))
    (setf (arguments t1) '(11)
          (argument t2) 22)
    (5am:is (= (argument t1) 11))
    (5am:is (= (argument t2) 22))
    (5am:is (equalp (arguments t1) '(11)))
    (5am:is (equalp (arguments t2) '(22))))
  (5am:signals simple-error
    (make-instance 'unary-term :argument 1 :arguments '(1) :context nil))
  (5am:signals simple-error
    (make-instance 'unary-term :arguments '(1 2) :context nil)))

#+5am
(5am:test (test-binary-term :compile-at :definition-time)
  (let ((t1 (make-instance 'binary-term :lhs 1 :rhs 2 :context nil))
        (t2 (make-instance 'binary-term :arguments '(3 4) :context nil)))
    (5am:is (= (lhs t1) 1))
    (5am:is (= (rhs t1) 2))
    (5am:is (= (lhs t2) 3))
    (5am:is (= (rhs t2) 4))
    (5am:is (equalp (arguments t1) '(1 2)))
    (5am:is (equalp (arguments t2) '(3 4)))
    (setf (arguments t1) '(11 22)
          (lhs t2) 33 (rhs t2) 44)
    (5am:is (= (lhs t1) 11))
    (5am:is (= (rhs t1) 22))
    (5am:is (= (lhs t2) 33))
    (5am:is (= (rhs t2) 44))
    (5am:is (equalp (arguments t1) '(11 22)))
    (5am:is (equalp (arguments t2) '(33 44))))
  (5am:signals simple-error
    (make-instance 'binary-term :lhs 1 :arguments '(1 2) :context nil))
  (5am:signals simple-error
    (make-instance 'binary-term :rhs 1 :arguments '(1 2) :context nil))
  (5am:signals simple-error
    (make-instance 'binary-term :arguments '(1) :context nil))
  (5am:signals simple-error
    (make-instance 'binary-term :arguments '(1 2 3) :context nil)))

#+5am
(5am:test (test-ternary-term :compile-at :definition-time)
  (let ((t1 (make-instance 'ternary-term :arg1 1 :arg2 2 :arg3 3 :context nil))
        (t2 (make-instance 'ternary-term :arguments '(4 5 6) :context nil)))
    (5am:is (= 1 (arg1 t1)))
    (5am:is (= 2 (arg2 t1)))
    (5am:is (= 3 (arg3 t1)))
    (5am:is (= 4 (arg1 t2)))
    (5am:is (= 5 (arg2 t2)))
    (5am:is (= 6 (arg3 t2)))
    (5am:is (equalp (arguments t1) '(1 2 3)))
    (5am:is (equalp (arguments t2) '(4 5 6)))
    (setf (arguments t1) '(11 22 33)
          (arg1 t2) 44 (arg2 t2) 55 (arg3 t2) 66)
    (5am:is (= 11 (arg1 t1)))
    (5am:is (= 22 (arg2 t1)))
    (5am:is (= 33 (arg3 t1)))
    (5am:is (= 44 (arg1 t2)))
    (5am:is (= 55 (arg2 t2)))
    (5am:is (= 66 (arg3 t2)))
    (5am:is (equalp (arguments t1) '(11 22 33)))
    (5am:is (equalp (arguments t2) '(44 55 66))))
  (5am:signals simple-error
    (make-instance 'ternary-term :arg1 1 :arguments '(1 2 3) :context nil))
  (5am:signals simple-error
    (make-instance 'ternary-term :arg2 1 :arguments '(1 2 3) :context nil))
  (5am:signals simple-error
    (make-instance 'ternary-term :arg3 1 :arguments '(1 2 3) :context nil))
  (5am:signals simple-error
    (make-instance 'ternary-term :arguments '(1 2) :context nil))
  (5am:signals simple-error
    (make-instance 'ternary-term :arguments '(1 2 3 4) :context nil)))

#+5am
(5am:test (test-starts-with-question-mark-p
           :compile-at :definition-time)
  (5am:is-true (starts-with-question-mark-p '?x))
  (5am:is-true (starts-with-question-mark-p '?a-very-long-variable-name))
  (5am:is-false (starts-with-question-mark-p '?))
  (5am:is-false (starts-with-question-mark-p 'foo)))

#+5am
(defun test-parse-binary-connective (term operator cc)
  (let ((parsed-term (parse-into-term-representation term cc)))
    (5am:is-true (typep parsed-term (term-type-for-operator operator cc)))
    (5am:is (eql operator (operator parsed-term)))
    (5am:is-true (typep (arguments parsed-term) 'cons))
    (5am:is (= 2 (length (arguments parsed-term))))
    (destructuring-bind (x y) (arguments parsed-term)
      (5am:is-true (typep x 'variable-term))
      (5am:is (eql '?x (name x)))
      (5am:is-true (typep y 'variable-term))
      (5am:is (eql '?y (name y))))))

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
        (5am:is (eql x (first (bound-variables parsed-term)))))
      (5am:is (eql '() (binding-keywords xb)))
      (5am:is (eql cc (binding-context xb))))
    (5am:is (eql 'unknown-general-application-term (type-of (argument parsed-term))))
    (5am:is-true (typep (arguments parsed-term) 'cons))
    (5am:is (= 1 (length (arguments parsed-term))))))

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
        (5am:is (eql y (second (bound-variables parsed-term)))))
      (5am:is (eql '() (binding-keywords xb)))
      (5am:is (eql '() (binding-keywords yb)))
      (5am:is (eql cc (binding-context xb)))
      (5am:is (eql cc (binding-context yb))))
    (5am:is (eql 'unknown-general-application-term (type-of (argument parsed-term))))
    (5am:is-true (typep (arguments parsed-term) 'cons))
    (5am:is (= 1 (length (arguments parsed-term))))))

#+5am
(5am:test (test-parse-into-term-representation
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
        (5am:is (eql '?y (name y)))))
    (test-parse-binary-connective '(and ?x ?y) 'and cc)
    (test-parse-binary-connective '(or ?x ?y) 'or cc)
    (test-parse-binary-connective '(implies ?x ?y) 'implies cc)
    (test-parse-binary-connective '(implied-by ?x ?y) 'implied-by cc)
    (test-parse-binary-connective '(iff ?x ?y) 'iff cc)
    (test-parse-quantification-1 '(foreach ?x (f ?x)) 'foreach cc)
    (test-parse-quantification-1 '(exists ?x (f ?x)) 'exists cc)
    (test-parse-quantification-2 '(foreach (?x ?y) (f ?x ?y))  'foreach cc)
    (test-parse-quantification-2 '(exists (?x ?y) (f ?x ?y))  'exists cc)))
