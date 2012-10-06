;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Names
;;; =====

(defclass name-mixin ()
  ((name
    :accessor name :initarg :name :initform :<unnamed> :type symbol
    :documentation "The name of the entity that inherits this mixin."))
  (:documentation
   "Mixin inherited by all classes that have names."))

(defclass required-name-mixin (name-mixin)
  ((name
    :initform (required-argument :name)
    :documentation "The name of the entity that inherits this mixin."))
  (:documentation
   "Mixin inherited by all classes that require a name."))

;;; Errors
;;; ======

;;; Define the RUNTIME-ERROR class here so that all other packages can
;;; derive from it.

(define-condition runtime-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "A runtime error occurred."))))

(define-condition invalid-class (runtime-error)
  ((expected-class :accessor expected-class :initarg :expected-class
                   :initform (required-argument :expected-class))
   (current-class :accessor current-class :initarg :current-class
                  :initform (required-argument :current-class)))
  (:report (lambda (condition stream)
             (format stream "~A is not an instance of ~A"
                     (class-name (current-class condition))
                     (class-name (expected-class condition))))))

(define-condition incompatible-sort-declarations (runtime-error)
  ((thing :initarg :thing)
   (sort-1 :initarg :sort-1)
   (sort-2 :initarg :sort-2))
  (:report (lambda (condition stream)
             (with-slots (thing sort-1 sort-2) condition
               (format stream "Incompatible sort declarations for ~W: ~W, ~W."
                       thing sort-1 sort-2)))))


(define-condition online-mode-error (runtime-error)
  ()
  (:documentation
   "Error raised if an operation is attempted that cannot be performed
   online."))

;;; Tracing
;;; =======

(defvar *trace-odysseus* t)

(defun trace-odysseus-p ()
  *trace-odysseus*)

(defun trace-odysseus ()
  (setf *trace-odysseus* t))

(defun untrace-odysseus ()
  (setf *trace-odysseus* nil))


;;; Information about the Lisp version
;;; ==================================

(defvar *features-for-lisp-types*
  '(("Clozure Common Lisp" :ccl :ccl-1.8 :clozure :clozure-common-lisp)
    ("SBCL" :sbcl)
    ("CMU Common Lisp" :cmucl :cmu :cmu20)))

(defun feature-for-lisp-type (&optional (lisp-type (lisp-implementation-type)))
  (or (second (assoc lisp-type *features-for-lisp-types* :test #'string-equal))
      (cerror "Return :UNKNOWN-LISP"
              "There is no known feature for ~A." lisp-type)
      :unknown-lisp))

;;; General utilities
;;; =================

(defmacro defglobal (name value &optional doc)
  `(#+sbcl sb-ext:defglobal
    #-sbcl defvar
    ,name ,value ,@(if doc (list doc) ())))

(defmacro gethash* (key hash-table default-value)
  (once-only (key hash-table)
    `(multiple-value-bind (value key-present-p)
	 (gethash ,key ,hash-table nil)
       (if (not key-present-p)
	   (let ((default ,default-value))
	     (setf (gethash ,key ,hash-table) default)
	     (values default nil))
	   (values value t)))))

(defun unquote (thing)
  (if (and (consp thing) (eql (first thing) 'quote))
      (second thing)
      thing))

(defun wrap-in-quote (thing)
  (if (or (null thing) (numberp thing) (keywordp thing) (stringp thing))
      thing
      (list 'quote thing)))

(defun wrap-in-forall (variables term)
  (if variables
      (list 'forall variables term)
      term))

(defun sexpr-equal-p (x y &optional (symbol-map (make-hash-table)))
  "This function is based on CCL's definition of EQUALP, and modified to be
useful for approximate comparison of sexprs for terms.  If X and Y are symbols
they are regarded as equal and stored in the symbol-map if they do not already
appear in the symbol map as different symbols."
  (cond ((eql x y) t)
        ((and (symbolp x) (symbolp y))
         (if (not (symbol-package x))
             (and (not (symbol-package y))
                  (string-equal (symbol-name x) (symbol-name y)))
             (if-let (x-val (gethash x symbol-map))
               (if (eq x-val y) t nil)
               (if-let (y-val (gethash y symbol-map))
                 (if (eq y-val x) t nil)
                 (progn (setf (gethash x symbol-map) y
                              (gethash y symbol-map) x)
                        t)))))
        ((characterp x) (and (characterp y) (eq (char-upcase x) (char-upcase y))))
        ((numberp x) (and (numberp y) (= x y)))
        ((consp x)
         (and (consp y)
              (sexpr-equal-p (car x) (car y) symbol-map)
              (sexpr-equal-p (cdr x) (cdr y) symbol-map)))
        ((pathnamep x) (equal x y))
        ((vectorp x)
         (and (vectorp y)
              (let ((length (length x)))
                (when (eq length (length y))
                  (dotimes (i length t)
                    (declare (fixnum i))
                    (let ((x-el (aref x i))
                          (y-el (aref y i)))
                      (unless (or (eq x-el y-el) (equalp x-el y-el))
                        (return nil))))))))
        ((arrayp x)
         (error "SEXPR-EQUAL-P for arrays not currently implemented."))
        ((and (typep x 'snark::variable) (typep y 'snark::variable))
         (eq x y))
        ((and (typep x 'structure-object) (typep y 'structure-object))
         (error "SEXPR-EQUAL-P for general structures not currently implemented."))
        ((and (hash-table-p x) (hash-table-p y))
         (error "SEXPR-EQUAL-P for hash tables not currently implemented."))
	((and (random-state-p x) (random-state-p y))
         (error "SEXPR-EQUAL-P for random states not currently implemented"))
        (t nil)))

(defun random-hex-list (length)
  (if (<= length 0)
      '()
      (cons (random 16) (random-hex-list (1- length)))))

(defun make-uuid (&optional (stream nil))
  "A simple (inefficient and probably incorrect) implementation of type 4
UUIDs."
  (format stream
          "~{~X~}-~{~X~}-4~{X~X~}-A~{~X~}-~{~X~}"
          (random-hex-list 8) (random-hex-list 4)
          (random-hex-list 3) (random-hex-list 3)
          (random-hex-list 12)))

(defun make-uuid-symbol (&optional (package (find-package '#:keyword)))
  (intern (uuid) package))

;;; Helper Methods for the MOP
;;; ==========================

(defun define-method (generic-function-name
                      &key (qualifiers '()) specializers lambda-list body)
  (let* ((gf (ensure-generic-function generic-function-name))
         (method-class (c2mop:generic-function-method-class gf)))
    (multiple-value-bind (fun initargs)
        (c2mop:make-method-lambda gf
                                  (c2mop:class-prototype method-class)
                                  body
                                  nil)
      (add-method gf
                  (apply #'make-instance method-class
                         :qualifiers qualifiers
                         :specializers specializers
                         :lambda-list lambda-list
                         :function (compile nil fun)
                         initargs)))))


;;; Helper Methods for Macro Definitions
;;; ====================================

(defun uncons (thing)
  (if (consp thing)
      (first thing)
      thing))

(defun process-argument-arglist (arglist)
  (multiple-value-bind (required optional rest keys)
      (parse-ordinary-lambda-list arglist :allow-specializers t)
    (assert (not rest) ()
            "Cannot currently handle arglists with &REST parameter.")
    `(,@(mapcar 'uncons required) ,@(mapcar 'first optional) ,@(mapcan 'first keys))))

;;; Three-Valued Logic
;;; ==================

;;; There are many properties of programs that cannot be decided
;;; during compile time.  To handle them in a uniform manner we
;;; introduce a three-valued logic.

(deftype boolean3 ()
  '(member t :unknown nil))

(defun and3 (&rest args)
  (let ((top-result t))
    (mapc (lambda (arg)
            (cond ((not arg)
                   (return-from and3 nil))
                  ((eql arg :unknown)
                   (setf top-result :unknown))))
          args)
    top-result))

(defun or3 (&rest args)
  (let ((bottom-result nil))
    (mapc (lambda (arg)
            (cond ((eql arg :unknown)
                   (setf bottom-result :unknown))
                  (arg
                   (return-from or3 t))))
          args)
    bottom-result))

;;; Testing
;;; =======


#+5am
(5am:def-suite odysseus-suite
  :description "The suite containing all tests for Odysseus.")

#+5am
(5am:def-suite odysseus-utilities-suite
  :in odysseus-suite
  :description "Tests for utilities.")

#+5am
(5am:def-suite odysseus-macro-suite
  :in odysseus-suite
  :description "Tests for macros.")

#+5am
(5am:def-suite odysseus-syntax-suite
  :in odysseus-suite
  :description "Tests for the syntax representation.")

#+5am
(5am:def-suite odysseus-situation-suite
  :in odysseus-suite
  :description "Tests for the situations.")

#+5am
(5am:def-suite odysseus-parser-suite
  :in odysseus-suite
  :description "Tests for the parser.")

#+5am
(5am:def-suite odysseus-interpreter-suite
  :in odysseus-suite
  :description "Tests for the interpreter.")

#+5am
(5am:def-suite odysseus-compiler-suite
  :in odysseus-suite
  :description "Tests for the compiler.")

#+5am
(5am:def-suite odysseus-builtins-suite
  :in odysseus-suite
  :description "Tests for the built-in predicates.")
