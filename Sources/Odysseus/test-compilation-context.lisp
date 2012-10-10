;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-syntax-suite)

(deftest test-enclosing-context-for-compilation-context ()
  (let ((cc (make-instance 'compilation-context)))
    (is (null (enclosing-context cc)))))

(deftest test-declare-operator-sort ()
  (let ((cu (make-instance 'compilation-unit)))
    (is (null (gethash 'foo (declared-operator-sorts cu) nil)))
    (declare-operator-sort 'foo 'bar cu)
    (is (eql 'bar (gethash 'foo (declared-operator-sorts cu) nil))))
  (let ((cu (make-instance 'compilation-unit))
        (*warn-for-null-operator-sorts* t))
    (signals warning
      (declare-operator-sort 'foo nil cu))
    (is (null (gethash 'foo (declared-operator-sorts cu) nil))))
  (let ((cu (make-instance 'compilation-unit))
        (*warn-for-null-operator-sorts* nil))
    (not-signals warning
      (declare-operator-sort 'foo nil cu))
    (is (null (gethash 'foo (declared-operator-sorts cu) nil)))))

(deftest test-primitive-action-definition ()
  (let* ((cu (make-instance 'compilation-unit))
         (pad (make-instance 'primitive-action-definition
                :signature '(t) :class 'primitive-action-term
                :operator 'foo :context cu)))
    ;; This is actually superfluous, since we are doing it in the
    ;; INITIALIZE-INSTANCE method.  It's only here to make the test easier to
    ;; understand.
    (setf (gethash 'foo (primitive-actions cu)) pad)
    (is (eql pad (primitive-action-definition 'foo cu)))
    (is (eql pad (primitive-action-definition 'foo cu nil)))
    (is (eql 'foobar (primitive-action-definition 'bar cu 'foobar)))
    (is (null (primitive-action-definition 'bar cu nil)))
    (signals no-definition-for-primitive-action
      (primitive-action-definition 'bar cu))))

(deftest test-setf-primitive-action-definition ()
  (let* ((cu (make-instance 'compilation-unit))
         (pad (make-instance 'primitive-action-definition
                :signature '(t) :class 'primitive-action-term
                :operator 'foo :context cu)))
    (setf (primitive-action-definition 'bar cu) pad)
    (is (eql pad (primitive-action-definition 'bar cu)))
    (is (eql pad (primitive-action-definition 'bar cu nil)))))


(deftest test-initialize-instance-for-primitive-action-definition ()
  (let* ((cu (make-instance 'compilation-unit))
         (pad (make-instance 'primitive-action-definition
                :signature '(t) :class 'primitive-action-term
                :operator 'foo :context cu)))
    (is (eql pad (primitive-action-definition 'foo cu)))
    (is (eql pad (primitive-action-definition 'foo cu nil))))
  (let* ((cu (make-instance 'compilation-unit))
         (pad (make-instance 'primitive-action-definition
                :signature '(t) :class 'primitive-action-term
                :operator 'foo :context cu
                :precondition '(exists (x y) (= x y)))))
    (is (typep (action-precondition pad) 'term))))
  
(deftest test-default-method-of-declare-primitive-action ()
  (let ((cu (make-instance 'compilation-unit)))
    (signals declaring-undefined-primitive-action
      (declare-primitive-action '#:undefined-primitive-action cu))
    ;; OK, so testing that continuing after a primitive action is probably
    ;; overkill...
    (is :continued
        (handler-case 
            (declare-primitive-action '#:another-undefined-primitive-action cu)
          (declaring-undefined-primitive-action (condition)
            (let ((restart (find-restart 'continue condition)))
              (when restart
                (invoke-restart restart :continued))))))))

(deftest test-define-primitive-action ()
  (let ((name '#:new-action)
        (term-class-name '#:new-action-term))
    (define-primitive-action name '(t t) :class-name term-class-name)
    (let ((cu (make-instance 'compilation-unit)))
      (is (eql name
               (operator (make-instance term-class-name :context cu))))
      (is (null (primitive-action-definition name cu nil)))
      (declare-primitive-action name cu)
      (is (typep (primitive-action-definition name cu)
                 'primitive-action-definition)))))


(deftest test-initialize-instance-for-fluent-definition ()
  (let* ((cu (make-instance 'compilation-unit))
         (fluent-def (make-instance 'fluent-definition
                       :operator 'foo :class 'foo-class :context cu)))
    (is (eql fluent-def (fluent-definition 'foo cu)))
    (is (eql fluent-def (fluent-definition 'foo cu nil))))
  (let* ((cu (make-instance 'compilation-unit))
         (fluent-def (make-instance 'fluent-definition
                       :operator 'foo :class 'foo-class :context cu
                       :successor-state '(= x y))))
    (is (typep (fluent-successor-state fluent-def) 'term))))


(deftest test-initialize-instance-for-relational-fluent-definition ()
  (let* ((cu (make-instance 'compilation-unit))
         (fluent-def (make-instance 'relational-fluent-definition
                       :operator 'foo :class 'foo-class :context cu)))
    (is (eql fluent-def (fluent-definition 'foo cu)))
    (is (eql fluent-def (fluent-definition 'foo cu nil))))
  (let* ((cu (make-instance 'compilation-unit))
         (fluent-def (make-instance 'relational-fluent-definition
                       :operator 'foo :class 'foo-class :context cu
                       :successor-state '(= x y))))
    (is (typep (fluent-successor-state fluent-def) 'term))))
  
(deftest test-default-method-of-declare-relational-fluent ()
  (let ((cu (make-instance 'compilation-unit))
        (name '#:undefined-fluent))
    (declare-relational-fluent name cu)
    (is (typep (fluent-definition name cu) 'relational-fluent-definition))))

(deftest test-define-relational-fluent ()
  (let ((name '#:new-fluent)
        (term-class-name '#:new-fluent-term))
    (define-relational-fluent name '(t t) :class-name term-class-name)
    (let ((cu (make-instance 'compilation-unit)))
      (is (eql name
               (operator (make-instance term-class-name :context cu))))
      (is (null (fluent-definition name cu nil)))
      (declare-relational-fluent name cu)
      (is (typep (fluent-definition name cu)
                 'relational-fluent-definition)))))


(deftest test-initialize-instance-for-functional-fluent-definition ()
  (let* ((cu (make-instance 'compilation-unit))
         (fluent-def (make-instance 'functional-fluent-definition
                       :operator 'foo :class 'foo-class :context cu)))
    (is (eql fluent-def (fluent-definition 'foo cu)))
    (is (eql fluent-def (fluent-definition 'foo cu nil))))
  (let* ((cu (make-instance 'compilation-unit))
         (fluent-def (make-instance 'functional-fluent-definition
                       :operator 'foo :class 'foo-class :context cu
                       :successor-state '(= x y))))
    (is (typep (fluent-successor-state fluent-def) 'term))))
  
(deftest test-default-method-of-declare-functional-fluent ()
  (let ((cu (make-instance 'compilation-unit))
        (name '#:undefined-fluent))
    (declare-functional-fluent name cu)
    (is (typep (fluent-definition name cu) 'functional-fluent-definition))))

(deftest test-define-functional-fluent ()
  (let ((name '#:new-fluent)
        (term-class-name '#:new-fluent-term))
    (define-functional-fluent name '(t t) :class-name term-class-name)
    (let ((cu (make-instance 'compilation-unit)))
      (is (eql name
               (operator (make-instance term-class-name :context cu))))
      (is (null (fluent-definition name cu nil)))
      (declare-functional-fluent name cu)
      (is (typep (fluent-definition name cu)
                 'functional-fluent-definition)))))
