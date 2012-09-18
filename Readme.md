*Iliad*
=======

Implementation of Logical Inference for Adaptive Devices
--------------------------------------------------------

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

To run *Iliad* you need:
* One of the supported Lisp implementations (CCL, CMUCL, ECL or SBCL,
  see below for the exact versions that we are using)
* ASDF (which should be included in the implementation of your choice)

Currently we do not provide scripts to run *Iliad* from the command line.

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
