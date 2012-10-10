;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)

(in-suite odysseus-situation-suite)

(deftest test-initial-situation ()
  (is (eq 's0 (to-sexpr (make-instance 'initial-situation)))))

(deftest test-successor-situation ()
  (let ((cc (make-instance 'compilation-unit)))
    (is (equalp '(do (say "Hi!") s0)
		    (to-sexpr
		     (make-instance
		      'successor-situation
		      :action (make-instance
			       'unknown-general-application-term
			       :operator 'say
			       :arguments (list (make-instance 'primitive-term
							       :value "Hi!"
							       :context cc))
			       :context cc)
		      :previous-situation (make-instance 'initial-situation)))))
    ;; Simpler way to write the test (but it depends on the parser):
    (is (equalp '(do (say "Hi!") s0)
		    (to-sexpr
		     (make-instance
		      'successor-situation
		      :action (parse-into-term-representation '(say "Hi!") cc)
		      :previous-situation (make-instance 'initial-situation)))))))

