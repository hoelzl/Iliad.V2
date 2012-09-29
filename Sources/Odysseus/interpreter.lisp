;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Runtime Errors
;;; ==============

(define-condition no-state-for-situation (runtime-error)
  ((situation :reader situation :initarg :situation))
  (:report (lambda (condition stream)
             (format stream "No state for situation ~A."
                     (situation condition)))))

(define-condition no-next-choice-point (runtime-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Tried to backtrack, but no choice point is left.")))) 

(define-condition online-mode-error (runtime-error)
  ())

(define-condition no-backtracking-in-online-mode (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "Cannot backtrack to a previous choice point in online mode."))))

(define-condition no-choice-point-creation-in-online-mode (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot create a choice point in online mode."))))

(define-condition unbound-variable-during-online-execution (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot execute actions containing variables in online mode."))))

;;; Interpreters
;;; ============

;;; We define a class INTERPRETER from which all interpreters inherit, and a
;;; class BASIC-INTERPRETER that implements the state management functions
;;; needed by most interpreters.

;;; The Class INTERPRETER
;;; ---------------------

(defclass interpreter ()
  ((context :accessor context :initarg :context
            :initform (make-instance 'compilation-unit)))
  (:documentation
   "The base class of all interpreters."))

(defgeneric reset-interpreter (interpreter &optional complete?)
  (:documentation
   "Resets the state of INTERPRETER.  When COMPLETE? is false, removes all
   choice points, saved states and other runtime information.  When COMPLETE?
   is true, it also clears all primitive actions, fluents, etc.")
  (:method ((interpreter interpreter) &optional (complete? nil))
    (when complete?
      (setf (context interpreter)
            (make-instance (class-of (context interpreter)))))))

(defgeneric interpreter-memento (interpreter)
  (:documentation
   "Returns a memento of INTERPRETER, i.e., its 'state' in a form that is
suitable for storing it in a choice point."))

(defgeneric (setf interpreter-memento) (new-memento interpreter)
  (:documentation
   "Restores INTERPRETER to the state in which NEW-MEMENTO was captured."))

(defgeneric prove (interpreter term &key solution-depth)
  (:documentation
   "Try to prove or refute TERM in INTERPRETER."))

(defgeneric can-execute-p (interpreter primitive-action-term situation)
  (:documentation
   "Returns true if it is possible to execute PRIMITIVE-ACTION-TERM in
   SITUATION, false otherwise."))

(defgeneric make-choice-point (interpreter term situation)
  (:documentation
   "Creates a new choice point for TERM in SITUATION."))

(defgeneric add-choice-point (interpreter term situation)
  (:documentation
   "Creates a new choice point by calling MAKE-CHOICE-POINT and adds it to the
   choice points of INTERPRETER."))

(defgeneric next-choice-point (interpreter)
  (:documentation
   "Returns the next available choice point."))

(defgeneric backtrack (interpreter &optional reason)
  (:documentation
   "Abandons the current computation and continues execution of the
   interpreter at the previous choice point.  Reason is a reason for
   diagnostic purposes."))

(defgeneric stored-actions (interpreter)
  (:documentation
   "The actions stored during the offline execution of the interpreter, in
    reverse order in which they should be executed."))

(defgeneric execute-stored-actions (interpreter)
  (:documentation
   "Executes all stored actions of INTERPRETER."))

(defgeneric execute-primitive-action (interpreter term)
  (:documentation
   "Execute the primitive action TERM for INTEPRETER."))

(defgeneric (setf stored-actions) (new-actions interpreter)
  (:documentation
   "Updates the action stored during offline execution."))

(defgeneric state-map (interpreter)
  (:documentation
   "Returns a map from situations to states."))

(defgeneric (setf state-map) (new-state-map interpreter)
  (:documentation
   "Updates the complete state-map of INTERPRETER."))

(defgeneric can-set-state-p (interpreter situation)
  (:documentation
   "Returns T if the state for INTERPRETER in SITUATION can be set, NIL
   otherwise.  When this method returns T it must be possible to execute
   (setf INTERPRETER SITUATION) successfully.")
  (:method ((interpreter interpreter) situation)
    (declare (ignore interpreter situation))
    nil))

(defgeneric state (interpreter situation &optional errorp)
  (:documentation
   "Returns the state of INTERPRETER in SITUATION.
If no state is available for SITUATION, STATE returns NIL if ERRORP is false,
raises an error otherwise.")
  (:method (interpreter situation &optional errorp)
    (let ((state (gethash situation (state-map interpreter) nil)))
      (or state
          (cond (errorp
                 (restart-case
                     (error 'no-state-for-situation :situation situation)
                   (provide-state (situation)
                     :test (lambda () (can-set-state-p interpreter situation))
                     :report "Provide a state for the situation."
                     :interactive (lambda ()
                                    (format t "Enter a new situation: ")
                                    (list (eval (read))))
                     (setf (state interpreter situation) situation))))
                (t nil))))))

(defgeneric (setf state) (new-state interpreter situation)
  (:documentation
   "Sets the state for INTERPRETER in SITUATION."))

(defmethod primitive-action-definition
    (action-name (interpreter interpreter) &optional default)
  (primitive-action-definition action-name (context interpreter) default))

(defmethod (setf primitive-action-definition)
    (new-value action-name (interpreter interpreter))
  (setf (primitive-action-definition action-name (context interpreter))
        new-value))

(defmethod the-empty-program-term ((interpreter interpreter))
  "Return the empty program term of INTERPRETER's context."
  (the-empty-program-term (context interpreter)))

(defmethod the-no-operation-term ((interpreter interpreter))
  "Return the no-operation term of INTERPRETER's context."
  (the-no-operation-term (context interpreter)))

(defmethod declare-primitive-action
    (operator (interpreter interpreter)
     &optional (class-name (symbolicate operator '#:-term)))
  (declare-primitive-action operator (context interpreter) class-name))

(defmethod declare-relational-fluent
  (operator (interpreter interpreter)
   &optional (class-name (symbolicate operator '#:-term)))
  (declare-relational-fluent operator (context interpreter) class-name))

(defmethod declare-functional-fluent
  (operator (interpreter interpreter)
   &optional (class-name (symbolicate operator '#:-term)))
  (declare-functional-fluent operator (context interpreter) class-name))



;;; The Class BASIC-INTERPRETER
;;; ---------------------------
              
(defclass top-level-context (compilation-unit)
  ()
  (:documentation
   "The state of a basic interpreter."))

(defclass choice-point ()
  ((term
    :accessor term :initarg :term
    :initform (required-argument :term)
    :documentation
    "The term that should be used to resume the execution.")
   (situation
    :accessor situation :initarg :situation
    :initform (required-argument :situation)
    :documentation "The situation in which the choice point was captured.")
   (interpreter-memento
    :accessor interpreter-memento :initarg :interpreter-memento
    :initform (required-argument :interpreter-memento)
    :documentation
    "The state of the interpreter at the time the choice point was captured.")))

#+(or)
(defclass repeated-choice-point (choice-point)
  ((max-repetitions :accessor max-repetitions :initarg :max-repetitions
                    :initform (required-argument :max-repetitions))
   (current-repetition :accessor current-repetition :initarg :current-repetition
                       :initform 0)))

(defclass basic-interpreter (interpreter)
  ((state-map
    :accessor state-map :initarg :state-map
    :initform (make-hash-table))
   (choice-points
    :accessor choice-points :initarg :choice-points
    :initform '()
    :documentation "A list of all available choice points.")
   (stored-actions
    :accessor stored-actions :initarg :stored-actions
    :initform '())
   (stored-continuations
    :accessor stored-continuations :initarg :stored-continuations
    :initform '())
   (onlinep 
    :accessor onlinep :initarg :onlinep :initform t
    :documentation "Returns T if the interpreter is in online mode."))
  (:default-initargs :context (make-instance 'top-level-context)))

(defmethod print-object ((self basic-interpreter) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "(~:[OFFLINE~;ONLINE~])"
            (onlinep self))))

(defmethod reset-interpreter
    ((interpreter basic-interpreter) &optional (complete? nil))
  (call-next-method interpreter complete?)
  (setf (state-map interpreter) (make-hash-table)
        (choice-points interpreter) '()
        (stored-actions interpreter) '()
        (stored-continuations interpreter) '()
        (onlinep interpreter) t))

(defclass basic-interpreter-memento ()
  ((stored-actions
    :accessor stored-actions :initarg :stored-actions
    :initform (required-argument :stored-actions))
   (stored-continuations
    :accessor stored-continuations :initarg :stored-continuations
    :initform (required-argument :stored-continuations))
   (onlinep
    :accessor onlinep :initarg :onlinep
    :initform (required-argument :onlinep)))
  (:documentation
   "The memento for a basic interpreter."))

(defmethod interpreter-memento ((interpreter basic-interpreter))
  (make-instance 'basic-interpreter-memento
                 :stored-actions (stored-actions interpreter)
                 :stored-continuations (stored-continuations interpreter)
                 :onlinep (onlinep interpreter)))

(defmethod (setf interpreter-memento)
    ((memento basic-interpreter-memento) (interpreter basic-interpreter))
  (setf (stored-actions interpreter)       (stored-actions memento)
        (stored-continuations interpreter) (stored-continuations memento)
        (onlinep interpreter)              (onlinep memento)))


(defvar *print-snark-timeouts* t)
(defvar *print-snark-undecidables* t)
(defvar *print-snark-refutations* t)

(defmethod prove ((interpreter basic-interpreter) (proof-term term)
                  &key (solution-depth 0))
  (let* ((free-variables (free-variables proof-term))
         (free-variable-sexprs (mapcar 'to-sexpr free-variables)))
    (multiple-value-bind (result reason answer)
        (prove-using-snark proof-term
                           :answer `(answer ,@free-variable-sexprs)
                           :context (context interpreter)
                           :solution-depth solution-depth)
      (when *trace-odysseus*
        (cond ((and (eql reason :refutation-found) *print-snark-refutations*)
               (format t "~&Refutation found for:~28T~:W~%" (to-sexpr proof-term)))
              ((and (eql reason :undecidable) *print-snark-undecidables*)
               (format t "~&Cannot decide:~28T~:W~%" (to-sexpr proof-term)))
              ((and (eql reason :timeout) *print-snark-timeouts*)
               (format t "~&Timeout while proving:~28T~:W~%" (to-sexpr proof-term)))))
      (values result reason free-variables answer))))


(defmethod can-execute-p
    ((interpreter basic-interpreter) (term primitive-action-term) situation)
  (let ((action-def (primitive-action-definition (operator term) interpreter)))
    (if (not (action-precondition action-def))
        ;; Terms without precondition can always execute.
        (values t :no-precondition '() '())
        ;; TERM has a precondition.  Pass it to Snark for evaluation.
        (let ((proof-term (make-instance 'unknown-general-application-term
                             :operator 'poss
                             :arguments (list term situation)
                             :context (context interpreter))))
          (prove interpreter proof-term)))))

(defmethod execute-stored-actions ((interpreter basic-interpreter))
  (let ((actions (nreverse (stored-actions interpreter)))
        (old-onlinep (onlinep interpreter)))
    (when actions
      (when *trace-odysseus*
        (format t "~&>>> Executing stored actions.~%"))
      (setf (onlinep interpreter) nil)
      (dolist (action actions)
        (execute-primitive-action interpreter action))
      (setf (onlinep interpreter) old-onlinep)
      (setf (stored-actions interpreter) '()))))

(defmethod make-choice-point
    ((interpreter interpreter) term situation)
  (when (onlinep interpreter)
    (cerror "Create the choice point anyway."
            'no-choice-point-creation-in-online-mode))
  (let ((cp (make-instance
             'choice-point
             :term term
             :situation situation
             :interpreter-memento (interpreter-memento interpreter))))
    ;; (push cp (choice-points interpreter))
    cp))

(defmethod add-choice-point
    ((interpreter basic-interpreter) term situation)
  (let ((cp (make-choice-point interpreter term situation)))
    (setf (choice-points interpreter)
          (append (choice-points interpreter) (list cp)))))

(defmethod next-choice-point ((interpreter basic-interpreter))
  (if (null (choice-points interpreter))
      (error 'no-next-choice-point)
      ;; This should be controllable by an execution strategy.
      (pop (choice-points interpreter))))

(defmethod backtrack ((interpreter interpreter) &optional reason)
  (let ((choice-point (next-choice-point interpreter)))
    (when (onlinep interpreter)
      (cerror "Backtrack anyway."
              'no-backtracking-in-online-mode))
    (when *trace-odysseus*
      (format t "~&~:[Backtracking~;~:*~A~].~%" reason))
    (setf (interpreter-memento interpreter)
          (interpreter-memento choice-point))
    (interpret-1 interpreter (term choice-point) (situation choice-point))))

(defmethod can-set-state-p ((interpreter basic-interpreter) situation)
  (declare (ignore interpreter situation))
  t)

(defmethod (setf state) (new-state (interpreter basic-interpreter) situation)
  (cond ((can-set-state-p interpreter situation)
         (setf (gethash situation (state-map interpreter))
               new-state))
        (t
         (cerror "Try setting the state anyway."
                 "Cannot set situation ~A in interpreter ~A."
                 situation interpreter)
         (setf (gethash situation (state-map interpreter))
               new-state))))



;;; Single-Step Interpretation
;;; ==========================

(defgeneric interpret-1 (interpreter term situation)
  (:documentation
   "Interpret a single execution step of TERM using INTERPRETER in SITUATION.
Returns the action performed by the term (an indirect instance of the class
PRIMITIVE-ACTION-TERM), the residual term that results from the evaluation
step, and the situation after executing the primitive action.  If no action
was performed by this evaluation step, an instance of NO-OPERATION-TERM is
returned as first argument."))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term list) situation)
  "Parse the program source into term representation and interpret the term."
  (interpret-1 interpreter
               (parse-into-term-representation term (context interpreter))
               situation))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term empty-program-term) situation)
  "Return a noop and another empty term (this is probably a bug)."
  (values (the-no-operation-term interpreter)
          term ;; FIXME: This should be an error term.
          situation))

(defun maybe-output-execution-trace-information
    (action term reason free-variables answer)
  (when *trace-odysseus*
    (if (or (eql reason :no-precondition)
            (null free-variables))
        (format t "~&~A~28T~:W~%    Reason:~28T~A~%"
                action (to-sexpr term) reason)
        (format t "~&~A~28T~:W~%    Reason:~28T~A~%    ~
                   Free Variables:~28T~:W~%    Answer:~28T~:W~%"
                action
                (to-sexpr term)
                reason
                (mapcar 'to-sexpr free-variables)
                answer))))

(defun perform-substitutions-in-interpreter
    (interpreter term situation free-variables answer)
  (let* ((local-context (make-instance 'local-context
                          :enclosing-context (context interpreter)))
         (new-terms
           (mapcar (lambda (exp)
                     (parse-into-term-representation exp local-context))
                   (rest answer))))
    (when new-terms
      (setf term (substitute-terms new-terms free-variables term)
            (stored-actions interpreter)
            (mapcar (lambda (action)
                      (substitute-terms new-terms free-variables action))
                    (stored-actions interpreter))
            (stored-continuations interpreter)
            (mapcar (lambda (continuation)
                      (substitute-terms new-terms free-variables continuation))
                    (stored-continuations interpreter)))
      (unless (onlinep interpreter)
        (setf situation (substitute-terms new-terms free-variables situation))))
    (values term situation)))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term test-term) situation)
  (when (< (solution-depth term) (max-solution-depth term))
    (add-choice-point interpreter
                      (clone-test-term-increasing-depth term)
                      situation))
  (multiple-value-bind (holds? reason free-variables answer)
      (prove interpreter (argument term)
             :solution-depth (solution-depth term))
    (cond (holds?
           (multiple-value-setq (term situation)
             (perform-substitutions-in-interpreter
              interpreter term situation free-variables answer))
           (maybe-output-execution-trace-information
            ">>> Successful test:" term reason free-variables answer)
           (values (the-no-operation-term interpreter)
                   (the-empty-program-term interpreter)
                   situation))
          (t
           (maybe-output-execution-trace-information
            ">>> Failed test:" term reason free-variables answer)
           (backtrack interpreter)))))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term primitive-action-term) situation)
  (multiple-value-bind (can-execute-p reason free-variables answer)
      (can-execute-p interpreter term situation)
    (cond (can-execute-p
           (multiple-value-setq (term situation)
             (perform-substitutions-in-interpreter
              interpreter term situation free-variables answer))
           (cond ((onlinep interpreter)
                  (maybe-output-execution-trace-information
                   ">>> Executing" term reason free-variables answer)
                  (values term
                          (the-empty-program-term interpreter)
                          (make-instance 'successor-situation
                            :action term
                            :previous-situation situation)))
                 (t
                  (maybe-output-execution-trace-information
                   "Storing:" term reason free-variables answer)
                  (push term (stored-actions interpreter))
                  (values (the-no-operation-term interpreter)
                          (the-empty-program-term interpreter)
                          (make-instance 'successor-situation
                            :action term
                            :previous-situation situation)))))
          (t
           (maybe-output-execution-trace-information
            "NOT Executing:" term reason free-variables answer)
           (backtrack interpreter)))))

(defvar *optimize-interpretation-of-declarations* t)

(defun interpret-1-body-term (interpreter term situation)
  (let ((body (body term)))
    ;; Since most programs start with a long list of declarations that do
    ;; nothing, pick them off here.  This makes the traces of INTERPRET-1 more
    ;; readable.  (It is also marginally faster, but the difference is
    ;; negligible).
    (when *optimize-interpretation-of-declarations*
      (iterate (repeat (length body))
        (if (typep (first body) 'declaration-term)
            (setf body (rest body))
            (leave))))
    (cond ((null body)
	   (interpret-1 interpreter
                        (the-empty-program-term interpreter)
                        situation))
	  (t
           (push (make-instance (class-of term)
                   :context (context term)
                   :source :generated-term
                   :body (rest body))
                 (stored-continuations interpreter))
	   (multiple-value-bind (action rest-term new-situation)
	       (interpret-1 interpreter (first body) situation)
             (values action
                     (if (is-final-term-p rest-term)
                         (the-empty-program-term interpreter)
                         (make-instance (class-of term)
                           :context (context term)
                           :source :generated-term
                           :body rest-term))
                     new-situation))))))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term sequence-term) situation)
  (interpret-1-body-term interpreter term situation))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term search-term) situation)
  (let ((old-onlinep (onlinep interpreter)))
    (setf (onlinep interpreter) nil)
    (multiple-value-bind (action rest-term new-situation)
        (interpret-1-body-term interpreter term situation)
      (setf (onlinep interpreter) old-onlinep)
      (values action
              rest-term
              new-situation))))

(defvar *permute-offline-choice* t)

(defmethod interpret-1
    ((interpreter basic-interpreter) (term action-choice-term) situation)
  (if (onlinep interpreter)
      (interpret-1 interpreter (random-elt (body term)) situation)
      (let ((choice-points
              (mapcar (lambda (choice)
                        (make-choice-point interpreter choice situation))
                      (if *permute-offline-choice*
                          (shuffle (body term))
                          (reverse (body term))))))
        (setf (choice-points interpreter)
              (append (choice-points interpreter) choice-points))
        (backtrack interpreter "Starting action choice"))))

(defmethod interpret-1
    ((interpreter basic-interpreter) (term declaration-term) situation)
  ;; NOTE: The implementation of INTERPRET-1 for sequence terms picks off
  ;; declarations at the start of sequences.  Therefore, if the implementation
  ;; of this method changes you need to adjust INTERPRET-1-BODY-TERM as well.
  (values (the-no-operation-term interpreter)
          (the-empty-program-term interpreter)
          situation))

;;; Interpretation
;;; ==============

;;; The following are possible variants for (multi-step) interpreters.

(defclass printing-interpreter (basic-interpreter)
  ((skip-noops :accessor skip-noops :initarg :skip-noops
               :initform t)))

(defmethod execute-primitive-action
    ((interpreter printing-interpreter) (term primitive-action-term))
  (when (and (onlinep interpreter) (contains-variable-p term))
    (cerror "Continue execution anyway."
            'unbound-variable-during-online-execution))
  (unless (and (skip-noops interpreter) (typep term 'no-operation-term))
    (format t "~&*** Performing Action ~28T~:W~60T**********~%"
            (to-sexpr term))))


;;; Default Interpreter
;;; ===================

(defvar *default-interpreter*
  (make-instance 'printing-interpreter))

(defun default-interpreter ()
  "Returns the default interpreter."
  *default-interpreter*)

(defun (setf default-interpreter) (new-interpreter)
  "Sets the default interpreter."
  (setf *default-interpreter* new-interpreter))

;;; Multi-Step Interpretation
;;; =========================

(defvar *suppress-interpretation-errors* t)

(defun interpret (term &key (interpreter (default-interpreter))
                            (situation (make-instance 'initial-situation))
                            (error-value nil))
  (labels ((recurse (term situation)
             (multiple-value-bind (action rest-term new-situation)
                 (interpret-1 interpreter term situation)
               (unless (typep action 'no-operation-term)
                 (assert (onlinep interpreter) ()
                         "Cannot execute actions while the interpreter is offline.")
                 #+(or)
                 (assert (null (stored-actions interpreter)) ()
                         "Should not have stored actions when trying to execute an action.")
                 (execute-stored-actions interpreter))
               (execute-primitive-action interpreter action)
               (if (is-final-term-p rest-term)
                   (cond ((stored-continuations interpreter)
                          (recurse (pop (stored-continuations interpreter))
                                   new-situation))
                         (t
                          (execute-stored-actions interpreter)
                          (values (to-sexpr new-situation) t)))
                   (recurse rest-term new-situation)))))
    (if *suppress-interpretation-errors*
        (handler-case
            (recurse term situation)
          (runtime-error (condition)
            (values (or error-value (class-name (class-of condition)))
                    nil)))
        (recurse term situation))))
