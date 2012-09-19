;;;; odysseus.asd

(asdf:defsystem #:odysseus
  :serial t
  :description "The core of the Iliad implementation of Poem"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #-(or ecl abcl) #:closer-mop
	       #:fiveam
               #:iterate
	       #:snark)
  :components ((:file "packages")
	       (:file "utilities")
	       (:file "macros")
	       (:file "syntax")
	       (:file "situation")
	       (:file "parser")
	       (:file "snark")
	       (:file "interpreter")))
