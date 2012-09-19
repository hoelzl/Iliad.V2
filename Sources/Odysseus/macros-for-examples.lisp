;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)

(defmacro defexample (name (&rest keys) &body term)
  `(make-instance 'odysseus-example
		  :name ',name
		  :term ',(if (null (rest term))
                              (first term)
                              (cons 'seq term))
                  :keys ',keys))
