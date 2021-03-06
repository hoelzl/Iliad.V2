;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:odysseus)
#+debug-odysseus
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Interpreters
;;; ============

;;; We define a class INTERPRETER from which all interpreters inherit, and a
;;; class BASIC-INTERPRETER that implements the state management functions
;;; needed by most interpreters.

;;; The Class INTERPRETER
;;; ---------------------

(defgeneric superordinate-interpreter (interpreter)
  (:documentation
   "A superordinate interpreter that uses INTERPRETER for various tasks.  This
   is used to return control to the superordinate interpreter for action that
   INTERPRETER can't handle itself."))

(defgeneric (setf superordinate-interpreter) (superordinate interpreter)
  (:documentation
   "Sets the superordinate interpreter of INTERPRETER to SUPERORDINATE."))

(defclass interpreter ()
  ((superordinate-interpreter
    :accessor superordinate-interpreter :initarg :subordinate-interpreter
    :initform nil)
   (interpreter-uuid
    :accessor interpreter-uuid :initarg :uuid
    :initform (make-uuid-symbol)))
  (:documentation
   "The base class of all interpreters."))

(define-condition cannot-interpret-term (runtime-error)
  ((interpreter :initarg :interpreter)
   (term :initarg :term))
  (:report (lambda (condition stream)
             (with-slots (interpreter term) condition
                 (format stream "Don't know how to interpret ~:W using ~A."
                         term interpreter)))))

(defgeneric interpret-1 (interpreter term situation)
  (:documentation
   "Interpret a single execution step of TERM using INTERPRETER in SITUATION.
Returns the action performed by the term (an indirect instance of the class
PRIMITIVE-ACTION-TERM), the residual term that results from the evaluation
step, and the situation after executing the primitive action.  If no action
was performed by this evaluation step, an instance of NO-OPERATION-TERM is
returned as first argument.")
  (:method ((interpreter interpreter) (term term) situation)
    (let ((superordinate (superordinate-interpreter interpreter)))
      (if superordinate
          (interpret-1 superordinate term situation)
          (error 'cannot-interpret-term
                 :interpreter interpreter
                 :term term)))))


(defgeneric reset-interpreter (interpreter &optional complete?)
  (:documentation
   "Resets the state of INTERPRETER.  When COMPLETE? is false, removes all
   choice points, saved states and other runtime information.  When COMPLETE?
   is true, it also clears all primitive actions, fluents, etc.")
  (:method ((interpreter interpreter) &optional (complete? nil))
    (when complete?
      (setf (context interpreter)
            (make-instance (class-of (context interpreter)))))))

(defgeneric can-continue-execution-p (interpreter)
  (:documentation
   "Returns true if INTERPRETER can continue execution one-step execution.
   The next term that should be processed is obtained by calling NEXT-TERM."))

(define-condition no-next-term (online-mode-error)
  ((interpreter :initarg :interpreter))
  (:report (lambda (condition stream)
             (with-slots (interpreter) condition
               (format stream "Interpreter ~A has no next term."
                       interpreter)))))

(defgeneric next-term (interpreter)
  (:documentation
   "Returns the next term for one-step execution of INTERPRETER or raises a
   condition of type NO-NEXT-TERM when no such term exists.  If
   CAN-CONTINUE-EXECUTION-P returns true for INTERPRETER, NEXT-TERM must not
   raise an error.")
  (:method ((interpreter interpreter))
    (error 'no-next-term :interpreter interpreter)))


(defgeneric onlinep (interpreter)
  (:documentation
   "Returns true, if INTERPRETER is currently in online mode."))

(defgeneric (setf onlinep) (new-value interpreter)
  (:documentation
   "Sets the online mode of INTERPRETER."))

(defgeneric try-to-finish-interpretation (interpreter situation)
  (:documentation
   "Tries to finish all outstanding undecidable proofs and executes all stored
   actions, if this is successful.  Returns a truth value indicating success
   and the new situation after substituting the values obtained during the
   proof."))

(defgeneric execute-stored-actions (interpreter)
  (:documentation
   "Executes all stored actions of INTERPRETER."))

(define-condition unbound-variable-during-online-execution (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot execute actions containing variables in online mode."))))

(define-condition cannot-execute-primitive-actions (runtime-error)
  ((interpreter :initarg :interpreter))
  (:report (lambda (condition stream)
             (with-slots (interpreter) condition
               (format stream "Interpreter ~A cannot execute primitive actions."
                       interpreter)))))

(defgeneric execute-primitive-action (interpreter term)
  (:documentation
   "Execute the primitive action TERM for INTEPRETER.")
  (:method ((interpreter interpreter)  term)
    (let ((superordinate (superordinate-interpreter interpreter)))
      (if superordinate
          (execute-primitive-action superordinate term)
          (error 'cannot-execute-primitive-actions :interpreter interpreter)))))


(define-condition no-choice-point-creation-in-online-mode (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot create a choice point in online mode."))))


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


(define-condition no-backtracking-in-online-mode (online-mode-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "Cannot backtrack to a previous choice point in online mode."))))

(defgeneric backtrack (interpreter &key reason continuation-function)
  (:documentation
   "Abandons the current computation and applies CONTINUATION-FUNCTION to
   INTERPRETER and the term and situation of the previous choice point.
   REASON is only for diagnostic purposes."))

(defmethod the-empty-program-term ((interpreter interpreter))
  "Return the empty program term of INTERPRETER's context."
  (the-empty-program-term (context interpreter)))

(defmethod the-no-operation-term ((interpreter interpreter))
  "Return the no-operation term of INTERPRETER's context."
  (the-no-operation-term (context interpreter)))


;;; Forwarded Definitions from Context
;;; ----------------------------------

(defmethod primitive-action-definition
    (action-name (interpreter interpreter) &optional default)
  (primitive-action-definition action-name (context interpreter) default))

(defmethod (setf primitive-action-definition)
    (new-value action-name (interpreter interpreter))
  (setf (primitive-action-definition action-name (context interpreter))
        new-value))


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
  ((context :accessor context :initarg :context
            :initform (make-instance 'compilation-unit))
   (state-map
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
   (deferred-proofs
    :accessor deferred-proofs :initarg :deferred-proofs
    :initform '())
   (onlinep 
    :accessor onlinep :initarg :onlinep :initform t
    :documentation "Returns T if the interpreter is in online mode."))
  (:default-initargs :context (make-instance 'top-level-context)))

;;; The Protocol for Basic Interpreters
;;; -----------------------------------

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

(defgeneric can-execute-p (interpreter primitive-action-term situation
                           &key solution-depth)
  (:documentation
   "Returns true if it is possible to execute PRIMITIVE-ACTION-TERM in
   SITUATION, false otherwise."))

(defgeneric stored-actions (interpreter)
  (:documentation
   "The actions stored during the offline execution of the interpreter, in
    reverse order in which they should be executed."))

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


(define-condition no-state-for-situation (runtime-error)
  ((situation :reader situation :initarg :situation))
  (:report (lambda (condition stream)
             (format stream "No state for situation ~A."
                     (situation condition)))))

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
                   (provide-state (state)
                     :test (lambda () (can-set-state-p interpreter situation))
                     :report "Provide a state for the situation."
                     :interactive (lambda ()
                                    (format t "Enter a new situation: ")
                                    (list (eval (read))))
                     (setf (state interpreter situation) state))))
                (t nil))))))

(defgeneric (setf state) (new-state interpreter situation)
  (:documentation
   "Sets the state for INTERPRETER in SITUATION."))


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
        (deferred-proofs interpreter) '()
        (onlinep interpreter) t))

(defmethod can-continue-execution-p ((interpreter basic-interpreter))
  (not (null (stored-continuations interpreter))))

(defmethod next-term ((interpreter basic-interpreter))
  (or (pop (stored-continuations interpreter))
      (call-next-method)))

(defclass basic-interpreter-memento ()
  ((interpreter-uuid
    :accessor interpreter-uuid :initarg :uuid
    :initform (required-argument :uuid))
   (state-map
    :accessor state-map :initarg :state-map
    :initform (required-argument :state-map))
   (stored-actions
    :accessor stored-actions :initarg :stored-actions
    :initform (required-argument :stored-actions))
   (stored-continuations
    :accessor stored-continuations :initarg :stored-continuations
    :initform (required-argument :stored-continuations))
   (deferred-proofs
    :accessor deferred-proofs :initarg :deferred-proofs
    :initform (required-argument :deferred-proofs))
   (onlinep
    :accessor onlinep :initarg :onlinep
    :initform (required-argument :onlinep)))
  (:documentation
   "The memento for a basic interpreter."))

(defmethod interpreter-memento ((interpreter basic-interpreter))
  (make-instance 'basic-interpreter-memento
    :uuid (interpreter-uuid interpreter)
    :state-map (copy-hash-table (state-map interpreter))
    :stored-actions (stored-actions interpreter)
    :stored-continuations (stored-continuations interpreter)
    :deferred-proofs (deferred-proofs interpreter)
    :onlinep (onlinep interpreter)))

(define-condition wrong-interpreter-uuid (runtime-error)
  ((interpreter :initarg :interpreter)
   (memento :initarg :memento))
  (:report (lambda (condition stream)
             (with-slots (interpreter memento) condition
               (format stream "UUID of interpreter ~A differs from Memento:~%  ~W~%  ~W."
                       interpreter
                       (interpreter-uuid interpreter)
                       (interpreter-uuid memento))))))

(defmethod (setf interpreter-memento)
    ((memento basic-interpreter-memento) (interpreter basic-interpreter))
  (unless (eql (interpreter-uuid interpreter) (interpreter-uuid memento))
    (cerror "Try to set the interpreter's state to the memento anyway."
            'wrong-interpreter-uuid))
  (setf (state-map interpreter)            (state-map memento)
        (stored-actions interpreter)       (stored-actions memento)
        (stored-continuations interpreter) (stored-continuations memento)
        (deferred-proofs interpreter)      (deferred-proofs memento)
        (onlinep interpreter)              (onlinep memento)))

(defvar *print-snark-refutations* t)
(defvar *print-snark-undecidables* t)
(defvar *print-snark-timeouts* t)

(defmethod prove ((interpreter basic-interpreter) (proof-term term)
                  &key (solution-depth 0)
                       (quantification-function 'universally-quantify))
  ;; TODO: Handle substitutions for the bound variables, and introduce
  ;; standard bound variable NOW for the current situation.
  (multiple-value-bind (closed-term bound-variables)
      (funcall quantification-function
               proof-term (context interpreter) :global nil)
    (let ((free-variables (free-variables proof-term))
          (bound-variable-sexprs (mapcar 'to-sexpr bound-variables)))
      (multiple-value-bind (result reason answer)
          (prove-using-snark (to-sexpr closed-term :include-global nil)
                             :answer-vars bound-variable-sexprs
                             :context (context interpreter)
                             :solution-depth solution-depth)
        (when *trace-odysseus*
          (cond ((and (eql reason :refutation-found) *print-snark-refutations*)
                 (format t "~&Refutation found for:~28T~:W~%" (to-sexpr proof-term)))
                ((and (eql reason :undecidable) *print-snark-undecidables*)
                 (format t "~&Cannot decide:~28T~:W~%" (to-sexpr proof-term)))
                ((and (eql reason :timeout) *print-snark-timeouts*)
                 (format t "~&Timeout while proving:~28T~:W~%" (to-sexpr proof-term)))))
        (values result reason free-variables answer)))))


(defmethod can-execute-p
    ((interpreter basic-interpreter) (term primitive-action-term) situation
     &key (solution-depth 0))
  (let ((action-def (primitive-action-definition (operator term) interpreter)))
    (if (not (action-precondition action-def))
        ;; Terms without precondition can always execute.
        (values t :no-precondition '() '())
        ;; TERM has a precondition.  Pass it to Snark for evaluation.
        (let ((proof-term (precondition-term term situation)))
          (prove interpreter proof-term
                 :solution-depth solution-depth
                 :quantification-function 'existentially-quantify)))))

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

(defvar *trace-choice-point-creation* nil)

(defmethod add-choice-point
    ((interpreter basic-interpreter) term situation)
  (when (and *trace-odysseus* *trace-choice-point-creation*)
    (format t "~&Creating choice point for~28T~:W~%    Situation:~28T~:W~%"
            term situation))
  (let ((cp (make-choice-point interpreter term situation)))
    (setf (choice-points interpreter)
          (append (choice-points interpreter) (list cp)))))


(define-condition no-next-choice-point (runtime-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Tried to backtrack, but no choice point is left.")))) 

(defmethod next-choice-point ((interpreter basic-interpreter))
  (if (null (choice-points interpreter))
      (error 'no-next-choice-point)
      ;; This should be controllable by an execution strategy.
      (pop (choice-points interpreter))))

(defmethod backtrack ((interpreter interpreter)
                      &key reason (continuation-function 'interpret-1))
  (let ((choice-point (next-choice-point interpreter)))
    (when (onlinep interpreter)
      (cerror "Backtrack anyway."
              'no-backtracking-in-online-mode))
    (when *trace-odysseus*
      (format t "~&~:[Backtracking~;~:*~A~].~%" reason))
    (setf (interpreter-memento interpreter)
          (interpreter-memento choice-point))
    (funcall continuation-function
             interpreter (term choice-point) (situation choice-point))))

(defmethod can-set-state-p ((interpreter basic-interpreter) situation)
  (declare (ignore interpreter situation))
  t)

(defmethod (setf state) (new-state (interpreter basic-interpreter) situation)
  (cond ((can-set-state-p interpreter situation)
         (setf (gethash situation (state-map interpreter))
               new-state))
        (t
         (cerror "Try setting the state anyway."
                 "Cannot set state for situation ~A in interpreter ~A."
                 situation interpreter)
         (setf (gethash situation (state-map interpreter))
               new-state))))


;;; Single-Step Interpretation
;;; ==========================

(defmethod interpret-1
    ((interpreter basic-interpreter) (term list) situation)
  "Parse the program source into term representation and interpret the term."
  (interpret-1 interpreter
               (parse-into-term-representation term (context interpreter))
               situation))


;;; Utilities for interpretation functions
;;; --------------------------------------

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
  "Substitutes the values for FREE-VARIABLES bound in ANSWER into TERM,
SITUATION and the stored actions and continuation of INTERPRETER."
  (let* ((local-context (make-instance 'local-context
                          :enclosing-context (context interpreter)))
         (new-terms
           (mapcar (lambda (exp)
                     (parse-into-term-representation exp local-context))
                   (rest answer))))
    (when new-terms
      (labels ((substitute-in (term)
                 (substitute-terms new-terms free-variables term))
               (substitute-all (list)
                 (mapcar #'substitute-in list)))
        (setf term 
              (substitute-in term))
        (setf (stored-actions interpreter) 
              (substitute-all (stored-actions interpreter)))
        (setf (stored-continuations interpreter)
              (substitute-all (stored-continuations interpreter)))
        (setf (deferred-proofs interpreter)
              (substitute-all (deferred-proofs interpreter)))
        (unless (onlinep interpreter)
          (setf situation (substitute-in situation)))))
    (values term situation)))


(defun proof-and-substitution-found
    (reason answer &key (ignore-variable-only-answers t))
  "Returns true if REASON indicates that a proof was found.  If
IGNORE-VARIABLE-ONLY-ANSWERS is true, then returns true only if a proof was
found and ANSWER contain at least one non-variable value."
  (when (and (eql reason :proof-found)
           (consp answer)
           (rest answer))
    (if ignore-variable-only-answers
        (not (every (lambda (elt)
                      (typep elt 'snark::variable))
                    (rest answer)))
        t)))

(defgeneric maybe-add-choice-point (interpreter term situation reason answer)
  (:documentation
   "Adds a choice point, if this may lead to further results.")
  
  (:method ((interpreter basic-interpreter) term situation reason answer)
    "The default method adds choice points only if a proof was found that
contains substitutions that do not only consist of variables."
    (if (not (onlinep interpreter))
        (cond ((proof-and-substitution-found reason answer)
               (add-choice-point
                interpreter
                (clone-multi-solution-term-increasing-depth term)
                situation)
               :proof-and-substitution-found)
              (t
               :no-proof-or-proof-and-no-substitution))
        :offline))

  (:method ((interpreter basic-interpreter) (term primitive-action-term)
                    situation reason answer)
    (declare (ignore situation reason answer))
    (if (action-precondition term)
        (call-next-method)
        :no-action-precondition)))



;;; Interpretation
;;; ==============

;;; The following are possible variants for (multi-step) interpreters.

(defclass printing-interpreter (interpreter)
  ((subordinate-interpreter
    :accessor subordinate-interpreter
    :initarg :subordinate-interpreter
    :initform (make-instance 'single-threaded-interpreter))
   (skip-noops
    :accessor skip-noops :initarg :skip-noops
    :initform t)))

(defmethod initialize-instance :after ((self printing-interpreter) &key)
  (setf (superordinate-interpreter (subordinate-interpreter self))
          self))


(define-delegates printing-interpreter subordinate-interpreter
  (context (interpreter))
  (interpret-1 (interpreter term situation))
  (reset-interpreter (interpreter &optional complete?))
  (can-continue-execution-p (interpreter))
  (next-term (interpreter))
  (onlinep (interpreter))
  ((setf onlinep) (new-value interpreter))
  (try-to-finish-interpretation (interpreter situation))
  (execute-stored-actions (interpreter))
  (make-choice-point (interpreter term situation))
  (add-choice-point (interpreter term situation))
  (next-choice-point (interpreter))
  (backtrack (interpreter &key reason (continuation-function 'interpret-1)))
  (the-empty-program-term (interpreter))
  (the-no-operation-term (interpreter)))

(defmethod execute-primitive-action
    ((interpreter printing-interpreter) (term primitive-action-term))
  (when (and (onlinep interpreter) (contains-variable-p term))
    (cerror "Continue execution anyway."
            'unbound-variable-during-online-execution))
  (unless (and (skip-noops interpreter) (typep term 'no-operation-term))
    (format t "~&*** Performing Action ~28T~:W~60T**********~%"
            (to-sexpr term))))

