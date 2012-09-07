;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-utilities)
#+5am
(5am:in-suite odysseus-macro-suite)

;;; Interning instances
;;; ===================

(defvar *compound-term-hash* (make-hash-table))

(defmacro define-interning-make-instance (base-name primary-key &optional secondary-key)
  (let* ((class-name (symbolicate base-name '#:-term))
         (accessor (symbolicate '#:lookup- base-name)))
    (if secondary-key
	`(defmethod make-instance :around ((class (eql (find-class ',class-name)))
                                           &key ,primary-key ,secondary-key
                                                (intern t) context)
	   (if (and intern context)
               (let ((instance (,accessor ,primary-key ,secondary-key context nil)))
                 (or instance
                     (setf (,accessor ,primary-key ,secondary-key context)
                           (call-next-method))))
	       (call-next-method)))
	`(defmethod make-instance :around ((class (eql (find-class ',class-name)))
                                           &key ,primary-key (intern t) context)
	   (if (and intern context)
               (let ((instance (,accessor ,primary-key context nil)))
                 (or instance
                     (setf (,accessor ,primary-key context)
                           (call-next-method))))
	       (call-next-method))))))

(in-package #:odysseus)
