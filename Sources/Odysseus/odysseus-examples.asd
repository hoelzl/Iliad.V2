;;; Examples for Odysseus 

(asdf:defsystem #:odysseus-examples
  :serial t
  :description "Examples for Odysseus"
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
  :components ((:file "macros-for-examples")
	       (:file "support-for-examples")
	       (:file "examples")))
