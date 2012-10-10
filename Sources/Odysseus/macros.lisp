;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))
(in-suite odysseus-macro-suite)

;;; Defining Delegates
;;; ==================

(defmacro defdelegate (name arglist from to &key new-value-type)
  "Delegate generic function NAME with arguments ARGLIST from the first
argument of type FROM to TO."
    (if (and (consp name) (eql (first name) 'setf))
        (let* ((new-value (first arglist))
               (new-arg (if new-value-type
                            (list new-value new-value-type)
                            new-value))
               (method-arglist `((,(second arglist) ,from) ,@(cddr arglist)))
               (argument-arglist `((,to ,(second arglist))
                                   ,@(process-argument-arglist (cddr arglist))))
               (name (second name)))
          `(defmethod (setf ,name)
               ,(cons new-arg method-arglist)
             (setf (,name ,@argument-arglist) ,new-value)))
        (let ((method-arglist `((,(first arglist) ,from) ,@(rest arglist)))
              (argument-arglist `((,to ,(first arglist))
                                  ,@(process-argument-arglist (rest arglist)))))
          `(defmethod ,name ,method-arglist
             (,name ,@argument-arglist)))))

(defmacro define-delegates (from to &rest name-arglist-pairs)
  `(progn
     ,@(mapcar (lambda (name-arglist-pair)
                 (destructuring-bind (name arglist &key new-value-type) name-arglist-pair
                   `(defdelegate ,name ,arglist ,from ,to
                      :new-value-type ,new-value-type)))
               name-arglist-pairs)))

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

;;; Maybe suppressing Snark output
;;; ==============================

(defmacro maybe-suppress-snark-output (&body body)
  `(if *print-snark-output*
       (progn ,@body)
       (snark:with-no-output
         ,@body)))

