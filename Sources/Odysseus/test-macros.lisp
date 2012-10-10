;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
(in-suite odysseus-macro-suite)

(deftest test-defdelegate ()
  (is (equalp '(defmethod interpret-1 ((interpreter foo) term situation)
		(interpret-1 (bar interpreter) term situation))
	      (macroexpand-1
	       '(defdelegate interpret-1 (interpreter term situation) foo bar))))
  (is (equalp '(defmethod prove ((interpreter foo) term 
				 &key solution-depth quantification-function)
		(prove (bar interpreter) term
		 :solution-depth solution-depth
		 :quantification-function quantification-function))
	      (macroexpand-1
	       '(defdelegate prove
		 (interpreter term &key solution-depth quantification-function)
		 foo bar)))))

(deftest test-define-delegates ()
  (is (equalp '(progn
		(defdelegate context (interpreter)
		 executing-interpreter subordinate-interpreter
		 :new-value-type nil)
		(defdelegate interpret-1 (interpreter term situation)
		 executing-interpreter subordinate-interpreter
		 :new-value-type nil)
		(defdelegate prove (interpreter term
				    &key solution-depth quantification-function)
                    executing-interpreter subordinate-interpreter
                    :new-value-type nil))
	      (macroexpand-1
	       '(define-delegates executing-interpreter subordinate-interpreter
		 (context (interpreter))
		 (interpret-1 (interpreter term situation))
		 (prove (interpreter term
			 &key solution-depth quantification-function)))))))
