;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-interpreter-suite)

(deftest test-interpret-1-single-threaded-empty-program-term ()
  (declare (optimize (debug 3)))
  (let* ((interp (make-instance 'single-threaded-interpreter))
	 (term (make-instance 'empty-program-term :context (context interp))))
    (multiple-value-bind (action situation substitution deferred-proofs continuations)
	(interpret-1 interp term (the-initial-situation))
      (is (eql (the-no-operation-term interp) action))
      (is (eql (the-initial-situation) situation))
      (is (eql (the-empty-substitution) substitution))
      (is (null deferred-proofs))
      (is (eql (the-empty-continuation-generator) continuations)))))

(deftest test-proof-and-substitution-found ()
  (is (eql nil (proof-and-substitution-found :undecidable '())))
  (is (eql nil (proof-and-substitution-found :proof-found '())))
  (is (eql nil (proof-and-substitution-found :proof-found '(answer))))
  (is (eql t (proof-and-substitution-found :proof-found '(answer a))))
  (is (eql t (proof-and-substitution-found :proof-found '(answer a b)))))

(deftest test-make-continuation-generator-02 ()
  (let* ((interp (make-instance 'single-threaded-interpreter))
	 (term (make-instance 'test-term :context (context interp))))
    (is (typep (make-continuation-generator
		interp term (the-initial-situation) '()
		:undecidable '())
	       'empty-continuation-generator))
    (is (typep (make-continuation-generator
		interp term (the-initial-situation) '()
		:proof-found '())
	       'empty-continuation-generator))
    (is (typep (make-continuation-generator
		interp term (the-initial-situation) '()
		:proof-found '(answer))
	       'empty-continuation-generator))
    (is (typep (make-continuation-generator
		interp term (the-initial-situation) '()
		:proof-found '(answer a b))
	       'list-continuation-generator))))

(deftest test-make-continuation-generator-03 ()
  (let* ((interp (make-instance 'single-threaded-interpreter))
	 (term (the-no-operation-term interp)))
    (is (typep (make-continuation-generator
		interp term (the-initial-situation) '()
		:proof-found '(answer a b))
	       'empty-continuation-generator))))


(deftest test-interpret-1-test-success ()
  (with-terms
    (let* ((interp (make-instance 'single-threaded-interpreter))
	   (test '(= a b))
	   (term (make-instance 'test-term
				:argument test
				:context (context interp)))
	   (reason :proof-found)
	   (free-variables (list x y))
	   (answer '(answer a b))
	   (deferred-proofs (list (the-empty-program-term interp)))
	   (*trace-odysseus* nil))
      (multiple-value-bind
	    (action situation substitution new-proofs cgen)
	  (interpret-1-test-success interp term (the-initial-situation)
				    :reason reason
				    :free-variables free-variables
				    :answer answer
				    :deferred-proofs deferred-proofs
				    :log-message "Running test")
	(is (eql action (the-no-operation-term interp)))
	(is (eql situation (the-initial-situation)))
	(is (equalp (list x y) (old-terms substitution)))
	(is (eql deferred-proofs new-proofs))
	(let ((continuations (continuations cgen)))
	  (is (= 1 (length continuations)))
	  (let ((cont (first continuations)))
	    (is (typep (term cont) 'test-term))
	    (is (eql test (argument (term cont))))
	    (is (= 1 (solution-depth (term cont))))
	    (is (eql (situation cont) (the-initial-situation)))
	    (is (null (deferred-proofs cont)))))))))
      
(deftest test-interpret-1-single-threaded-test-term-01 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 `(holds '(,relation-name ?x ?y)) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term interp) action))
	(is (eql (the-initial-situation) situation))
	(is (typep substitution 'non-empty-substitution))
	(is (equalp '(x y) (mapcar 'name (old-terms substitution))))
	(is (= 2 (length (new-terms substitution))))
	(is (null deferred-proofs))
	(is (eql (the-empty-continuation-generator) cgen))))))

(deftest test-interpret-1-single-threaded-test-term-02 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 `(holds '(not (,relation-name ?x ?y))) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (null action))
	(is (null situation))
	(is (typep substitution 'empty-substitution))
	(is (null deferred-proofs))
	(is (eql (the-empty-continuation-generator) cgen))))))


(deftest test-interpret-1-primitive-action-success ()
  (with-single-threaded-interpreter
    (let ((reason :proof-found)
	  (free-variables (list x y))
	  (answer '(answer a b))
	  (deferred-proofs (list (the-empty-program-term interp))))
      (multiple-value-bind
	    (action situation substitution new-proofs cgen)
	  (interpret-1-primitive-action-success interp action-term
						(the-initial-situation)
						:reason reason
						:free-variables free-variables
						:answer answer
						:deferred-proofs deferred-proofs)
	(is (typep action action-class-name))
	(is (typep situation 'successor-situation))
	(is (eql action (action situation)))
	(is (equalp (list x y) (old-terms substitution)))
	(is (eql deferred-proofs new-proofs))
	(let ((continuations (continuations cgen)))
	  (is (= 1 (length continuations)))
	  (let ((cont (first continuations)))
	    (is (typep (term cont) action-class-name))
	    (is (= 1 (solution-depth (term cont))))
	    (is (eql (situation cont) (the-initial-situation)))
	    (is (null (deferred-proofs cont)))))))))

      
(deftest test-interpret-1-single-threaded-primitive-action-term-01 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 `(,action-name ,x ,y) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (typep action action-class-name))
	(is (typep situation 'successor-situation))
	(is (eql action (action situation)))
	(is (equalp (list x y) (old-terms substitution)))
	(is (null deferred-proofs))
	(is (zerop (length (continuations cgen))))))))

(deftest test-interpret-1-single-threaded-primitive-action-term-02 ()
  (with-single-threaded-interpreter
    (define-primitive-action 'impossible-action '(t t)
      :precondition `(not (poss (impossible-action ?x ?y) ?s))
      :force-redefinition t)
    (declare-primitive-action 'impossible-action (context interp))
    (declare-operator-sort 'impossible-action '(t t) (context interp))
    (let ((action-term (make-instance 'impossible-action-term
				      :arguments (list x y)
				      :context (context interp))))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp action-term (the-initial-situation))
	(is (null action))
	(is (null situation))
	(is (typep substitution 'empty-substitution))
	(is (null deferred-proofs))
	(is (eql (the-empty-continuation-generator) cgen))))))

(deftest test-interpret-1-single-threaded-sequence-term-01 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 '(seq) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term interp) action))
	(is (typep situation 'initial-situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (zerop (length (continuations cgen))))))))

(deftest test-interpret-1-single-threaded-sequence-term-02 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 '(seq null) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term interp) action))
	(is (typep situation 'initial-situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (zerop (length (continuations cgen))))))))

(deftest test-interpret-1-single-threaded-sequence-term-03 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 '(seq null null) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term interp) action))
	(is (typep situation 'initial-situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (= 1 (length (continuations cgen))))
	(let ((cont (first (continuations cgen))))
	  (is (typep (term cont) 'sequence-term))
	  (is (= 1 (length (body (term cont)))))
	  (is (typep (first (body (term cont))) 'empty-program-term)))))))

(deftest test-interpret-1-single-threaded-search-term-01 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 '(search) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term interp) action))
	(is (typep situation 'initial-situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (zerop (length (continuations cgen))))))))

(deftest test-interpret-1-single-threaded-search-term-02 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 '(search null) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term interp) action))
	(is (typep situation 'initial-situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (zerop (length (continuations cgen))))))))

(deftest test-interpret-1-single-threaded-search-term-03 ()
  (with-single-threaded-interpreter
    (let ((term (parse-into-term-representation
		 '(search null null) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term interp) action))
	(is (typep situation 'initial-situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (= 1 (length (continuations cgen))))
	(let ((cont (first (continuations cgen))))
	  (is (typep (term cont) 'search-term))
	  (is (= 1 (length (body (term cont)))))
	  (is (typep (first (body (term cont))) 'empty-program-term)))))))

(deftest test-interpret-1-single-threaded-action-choice-term-01 ()
  (with-single-threaded-interpreter
    (let ((*permute-offline-choice* nil)
	  (term (parse-into-term-representation
		 '(choose (foo d) (bar b q)) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (null action))
	(is (null situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (typep cgen 'continuation-generator))
	(let ((continuations (continuations cgen)))
	  (is (= 2 (length continuations)))
	  (let ((cont-1 (first continuations))
		(cont-2 (second continuations)))
	    (is (eql 'foo (operator (term cont-1))))
	    (is (equalp '(d) (mapcar #'value (arguments (term cont-1)))))
	    (is (eql 'bar (operator (term cont-2))))
	    (is (equalp '(b q) (mapcar #'value (arguments (term cont-2)))))))))))

(deftest test-interpret-1-single-threaded-search-term-04 ()
  (with-single-threaded-interpreter
    (let ((*permute-offline-choice* nil)
	  (term (parse-into-term-representation
		 '(search (search null (bar b q)) null) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (eql (the-no-operation-term context) action))
	(is (eql (the-initial-situation) situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (typep cgen 'continuation-generator))
	(let ((continuations (continuations cgen)))
	  (is (= 2 (length continuations)))
	  (let ((cont-1 (first continuations))
		(cont-2 (second continuations)))
	    (is (eql 'search (operator (term cont-1))))
            (let ((body (body (term cont-1))))
              (is (= 1 (length body)))
              (is (typep (first body) 'empty-program-term)))
	    (is (eql 'search (operator (term cont-2))))
            (let ((body (body (term cont-2))))
              (is (= 2 (length body)))
              (is (eql 'search (operator (first body))))
              (let ((inner-body (body (first body))))
                (is (= 1 (length inner-body)))
                (is (eql 'bar (operator (first inner-body))))
                (is (equalp '(b q) (mapcar #'value (arguments (first inner-body))))))
              (is (typep (second body) 'empty-program-term)))))))))

(deftest test-interpret-1-single-threaded-search-term-05 ()
  (with-single-threaded-interpreter
    (let ((*permute-offline-choice* nil)
	  (term (parse-into-term-representation
		 '(search (choose (foo d) (bar b q)) null) context)))
      (multiple-value-bind (action situation substitution deferred-proofs cgen)
	  (interpret-1 interp term (the-initial-situation))
	(is (null action))
	(is (null situation))
	(is (eql (the-empty-substitution) substitution))
	(is (null deferred-proofs))
	(is (typep cgen 'continuation-generator))
	(let ((continuations (continuations cgen)))
	  (is (= 2 (length continuations)))
	  (let ((cont-1 (first continuations))
		(cont-2 (second continuations)))
	    (is (eql 'search (operator (term cont-1))))
            (let ((body (body (term cont-1))))
              (is (= 2 (length body)))
              (is (eql 'foo (operator (first body))))
              (is (equalp '(d) (mapcar #'value (arguments (first body)))))
              (is (typep (second body) 'empty-program-term)))
	    (is (eql 'search (operator (term cont-2))))
            (let ((body (body (term cont-2))))
              (is (= 2 (length body)))
              (is (eql 'bar (operator (first body))))
              (is (equalp '(b q) (mapcar #'value (arguments (first body)))))
              (is (typep (second body) 'empty-program-term)))))))))
