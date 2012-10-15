;;; odysseus-integration-tests

(asdf:defsystem #:odysseus-tests
  :serial t
  :description "Integration tests for Odysseus"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #+swank
	       #:hu.dwim.stefil+swank
	       #-swank
	       #:hu.dwim.stefil
               #:iterate
	       #:closer-mop
	       #:odysseus)
  :components ((:file "test-utilities")
	       (:file "macros-for-tests")
	       (:file "test-compilation-context")
	       (:file "test-terms")
	       (:file "test-compilation-unit")
	       (:file "test-situation")
	       (:file "test-substitution")
	       (:file "test-continuation")
	       (:file "test-parser")
	       (:file "test-interpreter")
	       (:file "test-single-threaded-interpreter")))
