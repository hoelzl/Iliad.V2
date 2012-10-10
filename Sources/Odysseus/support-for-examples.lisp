;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus-user)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defun interpret-one-step (term
                           &key (interpreter (default-interpreter))
                                (situation (make-instance 'initial-situation))
                                (error-value nil))
  "A variant of INTERPRET-1 that takes the same arguments as INTERPRET so that
it can be called as INTERPRET-FUNCTION of ODYSSEUS-EXAMPLE."
  (declare (ignore error-value))
  (multiple-value-bind (term situation substitution continuations)
      (interpret-1 interpreter term situation)
    (values term (list (to-sexpr situation) substitution continuations))))

(defvar *odysseus-examples*
  (make-hash-table))

(defun delete-example (name)
  (remhash name *odysseus-examples*))

(defun add-example (name example)
  (setf (gethash name *odysseus-examples*) example))

(defun find-example (name)
  (gethash name *odysseus-examples* nil))

(defclass odysseus-example ()
  ((name :accessor name :initarg :name
	 :initform (required-argument :name))
   (term :accessor term :initarg :term
	 :initform (required-argument :term))
   (interpret-function :accessor interpret-function
                       :initarg :interpret-function
                       :initform 'interpret)
   (set-up-function :accessor set-up-function :initarg :set-up-function
                    :initform (required-argument :set-up-function))
   (max-solution-depth :accessor max-solution-depth 
                       :initarg :max-solution-depth
                       :initform odysseus::*default-max-solution-depth*)
   (keys :accessor keys :initarg :keys
	 :initform '())
   (hidden? :accessor hidden? :initarg :hidden?
	    :initform nil))
  (:documentation
   "Data for an example that can be run by RUN-EXAMPLE."))

(defmethod print-object ((self odysseus-example) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (name self))))

(defmethod make-load-form ((self odysseus-example) &optional environment)
  (declare (ignore environment))
  `(make-instance (class-of self)
     :name ,(name self)
     :term ,(term self)
     :set-up-function ,(set-up-function self)
     :max-solution-depth ,(max-solution-depth self)
     :keys ,(keys self)
     :hidden? ,(hidden? self)))

(defmethod initialize-instance :after ((self odysseus-example) &key name)
  (add-example name self))

(defgeneric full-source-code (example)
  (:method ((example symbol))
    (when example
      (full-source-code (find-example example))))
  (:method ((example odysseus-example))
    (if (set-up-function example)
        (append (funcall (set-up-function example))
                (list (term example)))
        (term example))))

(defvar *odysseus-test-variables*
  '(od::*store-all-non-refuted-proof-terms*
    od::*continue-after-undecidable-precondition*
    od::*continue-after-undecidable-test*
    od::*optimize-interpretation-of-declarations*
    od::*permute-offline-choice*
    od::*instantiate-undecidable-choices*
    od::*assert-rewrite-for-declarations*
    od::*assert-rewrite-for-preconditions*
    od::*assert-rewrite-for-unique-names-axioms*
    od::*support-declarations*
    od::*support-preconditions*
    od::*support-unique-name-axioms*
    od::*unique-variable-counter*
    osnark::*run-time-limit*
    osnark::*ida-run-time-limit*
    osnark::*ida-iterations*
    osnark::*error-when-refutation-without-answer*))

(defun record-variable-values-for-test ()
  (iterate (for var in *odysseus-test-variables*)
    (collect (cons var (symbol-value var)))))

(defclass example-test-case ()
  ((test-example :accessor test-example :initarg :example
                 :initform (required-argument :example))
   (test-lisp-implementation-type :accessor test-lisp-implementation-type
                                  :initarg :lisp-implementation-type
                                  :initform (lisp-implementation-type))
   (test-lisp-implementation-version :accessor test-lisp-implementation-version
                                     :initarg :lisp-implementation-version
                                     :initform (lisp-implementation-version))
   (test-random-state :accessor test-random-state
                      :initarg :random-state
                      :initform (make-random-state nil))
   (test-variable-values :accessor test-variable-values
                         :initarg :variable-values
                         :initform (record-variable-values-for-test))
   (test-execution-mode :accessor test-execution-mode
                        :initarg :execution-mode
                        :initform (required-argument :execution-mode))
   (test-expected-result :accessor test-expected-result
                         :initarg :expected-result
                         :initform nil))
  (:documentation
   "Data for a single test case based on an example."))

(defmethod print-object ((self example-test-case) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (name (test-example self)))))

(defmethod make-load-form ((self example-test-case) &optional environment)
  (declare (ignore environment))
  ;; We assume that the tests are set up in the same way as when the example
  ;; was recorded, so we simply refer to the example by name.

  ;; Note that CMU Lisp cannot currently (in version 20c) read its printed
  ;; random states, therefore the generated result has to be edited by hand.
  `(make-instance ',(class-name (class-of self))
     :example (find-example ',(name (test-example self)))
     :lisp-implementation-type ,(test-lisp-implementation-type self)
     :lisp-implementation-version ,(test-lisp-implementation-version self)
     :random-state (make-random-state ,(test-random-state self))
     :variable-values ',(test-variable-values self)
     :execution-mode ,(test-execution-mode self)
     :expected-result ',(test-expected-result self)))

(defun write-example-test-case (test-case &optional (stream t))
  (if (typep test-case 'example-test-case)
      (let ((*print-case* :downcase)
            (*print-pretty* t))
        (format stream "~&#+~A~%~S~2%"
                (feature-for-lisp-type)
                (make-load-form test-case)))
      :skipping-non-test-case))

(defgeneric run-example (example &key execution-mode record-trace-p)
  (:documentation
   "Run EXAMPLE in EXECUTION-MODE (either :online or :offline)")

  (:method ((name symbol)
            &key (execution-mode :offline)
                 record-trace-p)
    (when name
      (run-example (find-example name)
                   :execution-mode execution-mode
                   :record-trace-p record-trace-p)))

  (:method ((test-case example-test-case)
            &key (execution-mode :offline)
                 record-trace-p)
    (declare (ignore execution-mode))
    (when record-trace-p
      (warn "Recording a trace while running a test case."))
    (let ((vars (test-variable-values test-case))
          (*random-state* (make-random-state (test-random-state test-case))))
      (progv (mapcar #'car vars) (mapcar #'cdr vars)
        (run-example (test-example test-case)
                     :execution-mode (test-execution-mode test-case)
                     :record-trace-p nil))))

  (:method ((example odysseus-example)
            &key (execution-mode :offline)
                 record-trace-p)
    (let ((interpreter (default-interpreter)))
      (reset-interpreter interpreter)
      (setf (onlinep interpreter) (eql execution-mode :online))
      (format t "~&Running example ~A in mode ~A.~%"
	      (name example) execution-mode)
      (format t "~&Source code: ~28T~:W~%" (term example))
      (let ((test-case (if record-trace-p
                           (make-instance 'example-test-case
                             :example example
                             :execution-mode execution-mode)
                           nil))
            (odysseus::*default-max-solution-depth* (max-solution-depth example)))
        (multiple-value-bind (success? result)
            (apply (interpret-function example)
                   (full-source-code example)
                   :interpreter interpreter
                   (keys example))
          (format t "~&~:[Execution terminated~;Result~]:~28T~:W~2&"
                  success? result)
          (cond (record-trace-p
                 (setf (test-expected-result test-case) result)
                 test-case)
                (t
                 (values success? (name example) result))))))))


(defun run-examples (examples &key (execution-mode :offline) record-traces-p)
  (mapcar (lambda (example)
            (setf example (find-example example))
            (if (or (null example) (hidden? example))
                :hidden-or-missing-example
                (run-example example
                             :execution-mode execution-mode
                             :record-trace-p record-traces-p)))
          examples))

(defun all-examples ()
  (let ((keys (copy-list (hash-table-keys *odysseus-examples*))))
    (sort keys (lambda (lhs rhs)
                 (string< (symbol-name lhs) (symbol-name rhs))))))


(defun run-all-examples (&key (execution-mode :offline) record-traces-p)
    (run-examples (all-examples)
                  :execution-mode execution-mode
                  :record-traces-p record-traces-p))

(defun record-output-for-single-example
    (example filename &key (execution-mode :offline))
  (let ((test-case (run-example example
                                :execution-mode execution-mode
                                :record-trace-p t)))
    (with-open-file (stream filename :direction :output :if-exists :append)
      (write-example-test-case test-case stream))))
    
(defun record-output-for-examples
    (examples filename &key (execution-mode :offline))
  (let ((test-cases (mapcar (lambda (example)
                              (run-example example
                                           :execution-mode execution-mode
                                           :record-trace-p t))
                            examples)))
    (with-open-file (stream filename :direction :output :if-exists :append
                            :if-does-not-exist :create)
      (mapc (lambda (test-case)
              (write-example-test-case test-case stream))
            test-cases))))

(defun filename-for-generated-tests ()
  (let ((*print-case* :downcase))
    (merge-pathnames
     (make-pathname :directory '(:relative "Generated-Tests")
                    :name (format nil "tests-for-~A" (feature-for-lisp-type))
                    :type "lisp")
     (directory-namestring (asdf:system-source-file :odysseus)))))

(defun record-all-examples (&key (filename (filename-for-generated-tests))
                                 (trace nil) (execution-mode :offline))
  (ensure-directories-exist filename)
  (let ((od::*trace-odysseus* trace))
    (record-output-for-examples (all-examples) filename
                                :execution-mode execution-mode)))

(defun read-generated-tests (&optional (filename (filename-for-generated-tests)))
  (iterate (for form in-file filename)
    (collect form)))

(defun execute-tests (tests
                      &key (trace nil) (suppress-all-output t)
                           (print-summary t) (execution-mode :offline))
  (let ((od::*trace-odysseus* trace)
        (successful-tests '())
        (failed-tests '()))
    (mapc (lambda (test)
            (when (typep test 'example-test-case)
              (multiple-value-bind (successp name result)
                  (if suppress-all-output
                      (snark:with-no-output
                        (run-example test :execution-mode execution-mode))
                      (run-example test :execution-mode execution-mode))
                (declare (ignore successp))
                (let ((expected (test-expected-result test)))
                  (if (sexpr-equal-p result expected)
                      (push (list name result) successful-tests)
                      (push (list name result expected) failed-tests))))))
          tests)
    (setf successful-tests (nreverse successful-tests)
          failed-tests (nreverse failed-tests))
    (when print-summary
      (format t "~&Executed ~A tests.~%  Success:~15T~A~%  Failure:~15T~A~2%"
              (+ (length successful-tests) (length failed-tests))
              (length successful-tests)
              (length failed-tests))
      (dolist (test failed-tests)
        (format t "~&Failed Test:~15T~A~%" (first test))
        (format t "~&  Expected:~15T~:W~&" (third test))
        (format t "~&  Got:~15T~:W~&" (second test))))
    #+(or)
    (values successful-tests
            failed-tests)))

(defun execute-all-tests (&key (filename (filename-for-generated-tests))
                               (trace nil) (suppress-all-output t)
                               (print-summary t) (execution-mode :offline))
  (let ((tests (mapcar 'eval (read-generated-tests filename))))
    (execute-tests tests
                   :trace trace :suppress-all-output suppress-all-output
                   :print-summary print-summary :execution-mode execution-mode)))



  

