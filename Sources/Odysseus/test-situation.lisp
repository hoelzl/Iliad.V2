;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-tests)

#+5am
(5am:in-suite odysseus-situation-suite)

#+5am
(5am:test (test-initial-situation :compile-at :definition-time)
  (5am:is (eq 's0 (to-sexpr (make-instance 'initial-situation)))))

#+5am
(5am:test (test-successor-situation :compile-at :definition-time)
  (let ((cc (make-instance 'compilation-unit)))
    (5am:is (equalp '(do (say "Hi!") s0)
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
    (5am:is (equalp '(do (say "Hi!") s0)
		    (to-sexpr
		     (make-instance
		      'successor-situation
		      :action (parse-into-term-representation '(say "Hi!") cc)
		      :previous-situation (make-instance 'initial-situation)))))))

