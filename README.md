futility
========

A collection of modern Fortran utilities. So far it includes some numerical
subroutine and a module for simplified input and output of arrays. In future
I hope to add various data structures, some useful functions for interacting
with the system (adding, removing files, etc.), and an object-oriented plotting
system.

Compiling
---------

futility uses [FoBiS](https://github.com/szaghi/FoBiS) for its build system.
This provides a simpler interface to work with than Makefiles. To compile,
simply execute

	./FoBiS.py build

and this will produce a shared-object file and place all module files in the
directory ``mod/``. If you want to recompile the documentation then type

	./FoBiS.py rule -ex makedoc

futility is known to compile with gfortran 4.8.2 on a 64 bit laptop running
Linux Mint Debian Edition. How it will behave anywhere else is not subject to
any guarantees.


Documentation
-------------

Extensive documentation of the various modules and procedures in Futility can
be found in the ``doc/`` directory. The library can be linked to your Fortran
programs in the usual way.
