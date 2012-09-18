*Iliad*
=======

<p align="right">
  <i>
    Rage—Goddess, sing the rage of Peleus' son Achilles,<br/>
    murderous, doomed, that cost the Achaeans countless losses,<br/>
    hurling down to the House of Death so many sturdy souls,<br/>
    great fighters’ souls, but made their bodies carrion,<br/>
    feasts for the dogs and birds,<br/>
    and the will of Zeus was moving toward its end.<br/>
    Begin, Muse, when the two first broke and clashed,<br/>
    Agamemnon lord of men and brilliant Achilles.<br/><br/>
  </i>
  —Homer. The Iliad.
</p>

*Iliad*, the *Implementation of Logical Inference for Adaptive
Devices* is the infrastructure for the *Poem* implementation.  

Odysseus
--------

<p align="right">
  <i>
    But Odysseus, cool tactician, tried to calm him:<br/>
    “Achilles, son of Peleus, greatest of the Achaeans,<br/>
	greater than I, stronger with spears by no small edge—<br/>
	yet I might just surpass you in seasoned judgment<br/>
	by quite a lot, since I have years on you<br/>
	and I know the world much better...<br/><br/>
  </i>
  —Homer. The Iliad.
</p>

*Odysseus* is the implementation of the core of the *Poem* language,
 providing support for specifying knowledge about the world, reasoning
 services and strategies.  It uses Mark Stickel's Snark theorem prover
 as the logical inference engine.

Installation and Execution
--------------------------

We currently provide neither precompiled packages nor scripts to run
*Iliad* from the command line.  Instead you have to download the
sources and run *Iliad* from within your Lisp implementation of
choice.

### Prerequisites

To run *Iliad* you need:

* One of the supported Lisp implementations (CCL, CMUCL, ECL or SBCL,
  see below for the exact versions that we are using).
* [ASDF 2](http://common-lisp.net/project/asdf/). This is already be
  included in all supported Lisp implementations.
* [Quicklisp](http://www.quicklisp.org/) (If you install the required
  packages by hand you could dispense with Quicklisp, but this is not
  something we would recommend.)

### Installing from sources

To install *Iliad* from sources so that it can be executed from within
your Lisp system:

* Download the sources of the latest *Iliad* release from the github
  page at https://github.com/hoelzl/Iliad/tags and unpack them in a
  location where ASDF can find them.  (To simplify updating to newer
  versions it's even better if you checkout the master branch with
  `git clone git://github.com/hoelzl/Iliad.git` or one of the other
  methods given on the project page (https://github.com/hoelzl/Iliad).
  If you are feeling adventurous and want to get the latest features
  you can checkout the development branch instead.)  To add additional
  directories to ASDF's search path you might have to add a form like  
  <pre>
    (:source-registry
        (:tree (:home "Prog/Lisp/Hacking/"))
        (:tree (:home "Prog/Lisp/Test-Projects/"))
        :inherit-configuration)</pre>
  to the file `~/.config/common-lisp/source-registry.conf`.
* Start your Lisp implementation and enter the following command:
  <pre>(asdf:load-system :iliad)</pre>
  This builds and loads all dependencies and the *Iliad* system.

### Running the tests

To run the test suite for *Odysseus*, enter the commands

    (asdf:load-system :odysseus-tests)
	(5am:run! 'odysseus:odysseus-suite)

### Working with the interpreters

You can work interactively with *Odysseus* in the following way:

    CL-USER> (asdf:load-system :iliad)
    T
    CL-USER> (in-package :odysseus-user)
    #<Package "ODYSSEUS-USER">
    ODYSSEUS-USER> (define-primitive-action eat)
    #<STANDARD-METHOD DECLARE-PRIMITIVE-ACTION ((EQL EAT) T)>
    ODYSSEUS-USER> (declare-primitive-action 'eat *default-interpreter-state*)
    #<PRIMITIVE-ACTION-DEFINITION #x302000FA29BD>
    ODYSSEUS-USER> (define-primitive-action sleep)
    #<STANDARD-METHOD DECLARE-PRIMITIVE-ACTION ((EQL SLEEP) T)>
    ODYSSEUS-USER> (declare-primitive-action 'sleep *default-interpreter-state*)
    #<PRIMITIVE-ACTION-DEFINITION #x30200100987D>
    ODYSSEUS-USER> (define-primitive-action celebrate)
    #<STANDARD-METHOD DECLARE-PRIMITIVE-ACTION ((EQL CELEBRATE) T)>
    ODYSSEUS-USER> (declare-primitive-action 'celebrate *default-interpreter-state*)
    #<PRIMITIVE-ACTION-DEFINITION #x302001030B5D>
    ODYSSEUS-USER> (interpret-and-print
    		'(seq
    		  (eat something)
    		  (sleep several hours)
    		  (celebrate)))
    
    (EAT SOMETHING) 
    (SLEEP SEVERAL HOURS) 
    (CELEBRATE) 
    :DONE
    ODYSSEUS-USER> 


Supported Lisp Implementations
------------------------------

*Iliad* has been tested on the folowing implementations running on OSX:

* Armed Bear Common Lisp: 1.0.1-svn-13750-13751
* Clozure Common Lisp: Version 1.8-r15378M  (DarwinX8664)
* CMU Common Lisp: 20c release-20c (20C Unicode)
* ECL: 12.7.1
* SBCL: 1.0.58

It should be straightforward to port *Iliad* to other Lisp
implementations or operating systems.

The Workflow for the *Poem* Project
-----------------------------------

<p align="right">
  <i>
    The work done, the feast laid out, they ate well<br/>
    and no man’s hunger lacked a share of the banquet.<br/><br/>
  </i>
  —Homer. The Iliad.
</p>

I am currently experimenting with the git-flow branching model for
*Poem*.  Please see the article "[A successful Git branching
model](http://nvie.com/posts/a-successful-git-branching-model/)" for
details.


About this Document
-------------------

<p align="right">
  <i>
    No one should ever let such nonsense pass his lips,<br/>
    no one with any skill in fit and proper speech—<br/>
  </i>
  —Homer. The Iliad.
</p>


All quotes from the Iliad are taken from the Penguin Classics edition
translated by Robert Fagles.  The HTML markup for the quotes uses the
old-fashioned ```align="right"``` style, since CSS does currently not
seem to work for the Github renderer.
