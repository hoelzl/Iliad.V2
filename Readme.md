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
Devices* is the infrastructure for the *Poem* language.  *Poem*
is the high-level modeling language of the
[ASCENS](http://www.ascens-ist.eu) project.

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

You interact with *Iliad* from the read-eval-print loop of your Lisp
system.  To install *Iliad* from sources you have to perform the
following steps.

* Download the Snark theorem prover.  The original sources are
  available from
  [Mark Stickel's page](http://www.ai.sri.com/~stickel/snark.html),
  however it is probably easier if you get the copy from
  [my github page](https://github.com/hoelzl/Snark).  The latter
  version contains ASDF system definitions which simplify the
  integration with *Iliad*.  Unpack the sources in a location where
  ASDF can find them.  To add additional directories to ASDF's search
  path you might have to add a form like
  <pre>
    (:source-registry
        (:tree (:home "Prog/Lisp/Hacking/"))
        (:tree (:home "Prog/Lisp/Imported-Projects/"))
        :inherit-configuration)</pre>
  to the file `~/.config/common-lisp/source-registry.conf`.
* Download the sources of the latest *Iliad* release from the github
  page at https://github.com/hoelzl/Iliad/tags and unpack them in a
  location where ASDF can find them.  (To simplify updating to newer
  versions it's even better if you checkout the master branch with
  `git clone git://github.com/hoelzl/Iliad.git` or one of the other
  methods given on the project page (https://github.com/hoelzl/Iliad).
  If you are feeling adventurous and want to get the latest features
  you can checkout the development branch instead.)  
* Start your Lisp implementation and enter the following command:
  <pre>(asdf:load-system :iliad)</pre>
  This builds and loads all dependencies and the *Iliad* system.

### Running the tests

To run the test suite for *Odysseus*, enter the commands

    (asdf:load-system :odysseus-tests)
	(5am:run! 'odysseus:odysseus-suite)

### Working with the interpreters

The *Iliad* implementation contains a number of examples.  You can
run the examples for *Odysseus* in the following way:

    CL-USER> (asdf:load-system :odysseus-examples)
    T
    CL-USER> (in-package :odysseus-user)
    #<Package "ODYSSEUS-USER">
    ODYSSEUS-USER> (run-example 'interpret-16)
    Running example INTERPRET-16 in mode ONLINE.
    Source code:                (SEARCH (WORK ANNABELLE)
                                        (CHOOSE
                                         (HOLDS (FEMALE ?P.PERSON))
                                         (HOLDS (MALE ?P.PERSON)))
                                        (CELEBRATE ?P.PERSON)
                                        (HOLDS (MALE ?P.PERSON)))
    Storing:                    (WORK ANNABELLE)
        Reason:                 NO-PRECONDITION
    Starting action choice.
    Trying to prove or refute:  (MALE #:?P2924.PERSON)
    >>> Successful test:        (HOLDS? (MALE MATTHIAS))
        Reason:                 PROOF-FOUND
        Free Variables:         (#:?P2924.PERSON)
        Answer:                 (ANSWER MATTHIAS)
    Trying to prove or refute:  (POSS (CELEBRATE MATTHIAS)
                                 (DO (WORK ANNABELLE) S0 ))
    Cannot decide:              (POSS (CELEBRATE MATTHIAS)
                                 (DO (WORK ANNABELLE) S0 ))
    NOT Executing:              (CELEBRATE MATTHIAS)
        Reason:                 UNDECIDABLE
    Backtracking.
    Trying to prove or refute:  (FEMALE #:?P2924.PERSON)
    >>> Successful test:        (HOLDS? (FEMALE ANNABELLE))
        Reason:                 PROOF-FOUND
        Free Variables:         (#:?P2924.PERSON)
        Answer:                 (ANSWER ANNABELLE)
    Trying to prove or refute:  (POSS (CELEBRATE ANNABELLE)
                                 (DO (WORK ANNABELLE) S0 ))
    Refutation found for:       (POSS (CELEBRATE ANNABELLE)
                                 (DO (WORK ANNABELLE) S0 ))
    NOT Executing:              (CELEBRATE ANNABELLE)
        Reason:                 REFUTATION-FOUND
    Backtracking.
    Trying to prove:            (MALE #:?P2924.PERSON)
    >>> Successful test:        (HOLDS? (MALE LENZ))
        Reason:                 PROOF-FOUND
        Free Variables:         (#:?P2924.PERSON)
        Answer:                 (ANSWER LENZ)
    Trying to prove or refute:  (POSS (CELEBRATE LENZ)
                                 (DO (WORK ANNABELLE) S0 ))
    Refutation found for:       (POSS (CELEBRATE LENZ)
                                 (DO (WORK ANNABELLE) S0 ))
    NOT Executing:              (CELEBRATE LENZ)
        Reason:                 REFUTATION-FOUND
    Backtracking.
    Trying to prove:            (FEMALE #:?P2924.PERSON)
    Cannot decide:              (FEMALE #:?P2924.PERSON)
    >>> Failed test:            (HOLDS? (FEMALE #:?P2924.PERSON)
                                 :SOLUTION-DEPTH 1)
        Reason:                 UNDECIDABLE
        Free Variables:         (#:?P2924.PERSON)
        Answer:                 NIL
    Backtracking.
    Trying to prove:            (MALE #:?P2924.PERSON)
    >>> Successful test:        (HOLDS? (MALE LAITH))
        Reason:                 PROOF-FOUND
        Free Variables:         (#:?P2924.PERSON)
        Answer:                 (ANSWER LAITH)
    Trying to prove or refute:  (POSS (CELEBRATE LAITH)
                                 (DO (WORK ANNABELLE) S0 ))
    Storing:                    (CELEBRATE LAITH)
        Reason:                 PROOF-FOUND
    Trying to prove or refute:  (MALE LAITH)
    >>> Successful test:        (HOLDS? (MALE LAITH))
        Reason:                 PROOF-FOUND
    >>> Executing stored actions.
    *** Performing Action       (WORK ANNABELLE)                **********
    *** Performing Action       (CELEBRATE LAITH)               **********
    Result:                     (DO (CELEBRATE LAITH)
                                    (DO (WORK ANNABELLE) S0) )
    
    NIL
    ODYSSEUS-USER> 

The actual execution of the program on your system may differ since
the default execution mode of *Odysseus* is random resolution of
non-deterministic choices, however the final answer should always be
the same as shown in the example.  Look into the file
`Sources/Odysseus/examples.lisp` to see how to write your own
specifications or programs.  More documentation will be forthcoming in
the near future.

Supported Lisp Implementations
------------------------------

*Iliad* has been tested on the folowing implementations running on OSX:

* Clozure Common Lisp: Version 1.8-r15378M  (DarwinX8664)
* CMU Common Lisp: 20c release-20c (20C Unicode)
* SBCL: 1.0.58

It should be straightforward to port *Iliad* to other Lisp
implementations or operating systems.

The following Lisp implementations don't seem to work:

* Armed Bear Common Lisp: 1.0.1-svn-13750-13751: Seems to run into the
  `java.lang.OutOfMemoryError: PermGen space` error no matter how much
  memory I allocate on the command line.  Tips for solving this
  problem are very welcome.
* ECL: 12.7.1: Fails with an internal error while compiling Snark.
  
I haven't tried running *Iliad* on LispWorks, ACL, CLisp or any other
Lisp implementation.  Reports and bug fixes are always welcome.


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
*Poem*.  Please see the article
"[A successful Git branching model](http://nvie.com/posts/a-successful-git-branching-model/)"
for details.  Furthermore, I try to write the commit messages loosely
based on the
[Note About Git Commit Messages](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
from tbaggery.


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
