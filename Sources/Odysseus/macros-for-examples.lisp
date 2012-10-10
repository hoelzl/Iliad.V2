;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defmacro defexample (name (&key hidden? 
                                 (max-solution-depth odysseus::*default-max-solution-depth*)
                                 (interpret-function 'interpret)
                                 set-up-function
                                 keys)
                      &body term)
  `(make-instance 'odysseus-example
		  :name ',name
		  :term ',(if (null (rest term))
                              (first term)
                              (cons 'seq term))
                  :max-solution-depth ,max-solution-depth
                  :keys ,keys
                  :set-up-function ,set-up-function
                  :interpret-function ,interpret-function
                  :hidden? ,hidden?))

