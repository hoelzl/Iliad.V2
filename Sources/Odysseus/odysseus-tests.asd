;;; odysseus-integration-tests

(asdf:defsystem #:odysseus-tests
  :serial t
  :description "Integration tests for Odysseus"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #:fiveam
               #:iterate
	       #:odysseus)
  :components ((:file "test-utilities")
	       (:file "test-syntax")))
