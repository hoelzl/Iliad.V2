;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defmacro with-terms (&body body)
  `(let* ((cu (make-instance 'compilation-unit))
	  (term (make-instance 'term :context cu))
	  (var1 (make-instance 'variable-term
                  :name 'var1
                  :sort 'var1-sort
                  :context cu))
          (var2 (make-instance 'variable-term
                  :name 'var2
                  :sort 'var2-sort
                  :is-bound-p t
                  :context cu))
          (zero (make-instance 'number-term
                  :value 0
                  :context cu))
          (x (parse-into-term-representation '?x.x-sort cu))
          (y (parse-into-term-representation '?y.y-sort cu))
          (z (parse-into-term-representation '?z.z-sort cu))
          (call-term (parse-into-term-representation
                      '(f ?x.x-sort ?y.y-sort) cu))
          (call-term-0 (parse-into-term-representation
                        '(f0) cu))
          (call-term-1 (parse-into-term-representation
                        '(f1 ?x.x-sort) cu))
          (call-term-2 (parse-into-term-representation
                        '(f2 ?x.x-sort ?y.y-sort) cu))
          (call-term-3 (parse-into-term-representation
                        '(f3 ?x.x-sort ?y.y-sort ?z.z-sort) cu))
          (situation (make-instance 'successor-situation
                       :action call-term
                       :previous-situation (make-instance 'initial-situation)))

          (subst (make-instance 'substitution 
                   :old-terms (list x z)
                   :new-terms (list var1 var2))))
     (declare (ignorable term var1 var2 zero x y z
                         call-term call-term-0 call-term-1 call-term-2 call-term-3
                         subst situation))
     ,@body))
