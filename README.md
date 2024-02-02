newLISP mirror
==============

[![Build Status](https://travis-ci.org/kosh04/newlisp.svg?branch=develop)](https://travis-ci.org/kosh04/newlisp)
[![Build status](https://ci.appveyor.com/api/projects/status/qg6ijtx867q5fxnl/branch/develop?svg=true)](https://ci.appveyor.com/project/kosh04/newlisp/branch/develop)
[![codecov](https://codecov.io/gh/kosh04/newlisp/branch/develop/graph/badge.svg)](https://codecov.io/gh/kosh04/newlisp)
[![DebianBadge](https://badges.debian.net/badges/debian/stable/newlisp/version.svg)](https://packages.debian.org/stable/newlisp) <!-- OR https://buildd.debian.org/status/package.php?p=newlisp -->
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/kosh04/newlisp)

## newLISP with Git(Hub)

1. What is newLISP and what can I do with it?

  newLISP is a LISP-like scripting language for doing things you typically do with scripting languages: programming for the internet, system administration, text processing, gluing other programs together, etc. newLISP is a scripting LISP for people who are fascinated by LISP's beauty and power of expression, but who need it stripped down to easy-to-learn essentials.

2. What makes newLISP so special, why 'new'?

  LISP is an old language born, grown, and standardized in times very different from today, times when programming was for highly educated people who engineered programs. newLISP is LISP reborn as a scripting language: pragmatic and casual, simple to learn without requiring you to know advanced computer science concepts. Like any good scripting language, newLISP is quick to get into and gets the job done without fuss.
  
  newLISP has a very fast startup time, is small on resources like disk space and memory and has a deep, practical API with functions for networking, statistics, machine learning, regular expressions, multiprocessing and distributed computing built right into it, not added as a second thought in external modules.
  
  
See also [the Frequently Asked Questions (FAQ) page](http://www.newlisp.org/index.cgi?FAQ).



NOTE: This repository is *unofficial* mirror.

This is a newLISP experimental repository.
All source files are fetched from [SourceForge](http://sourceforge.net/projects/newlisp/files/).
Mainly available for easy browsing of the source code, to view development history, create a patch, etc.


### branch

- `master`  : mirror of original newlisp sources
- `develop` : `master` files and `README.md`, `.travis.yml`, `appveyor.yml` plus miscellaneous patches  (default branch)
- `feature/xxx` : (reserved)

### features

- [Source Archives](https://github.com/kosh04/newlisp/releases)
- [Travis CI](https://travis-ci.org/kosh04/newlisp) : Continuous Integration
- [AppVeyor](https://ci.appveyor.com/project/kosh04) : Continuous Integration (for Windows)
- [Codecov](https://codecov.io/gh/kosh04/newlisp) : Code Coverage
- [Gitter](https://gitter.im/kosh04/newlisp) : Chat about this repo

## Contribute

Issue and pull requests about this repository are welcome.

For code contributions, bug fixes, reports and comments,
please contact Lutz Mueller (see README) or post to the Forum.

## Link

- newLISP http://www.newlisp.org/
- newLISP Forum http://www.newlispfanclub.alh.net/

## License

newLISP and Nuevatec are trademarks of Lutz Mueller.
All files are distribute by GPLv3. For more details,
see [doc/COPYING](https://github.com/kosh04/newlisp/blob/master/doc/COPYING) and [doc/License.html](https://rawgit.com/kosh04/newlisp/master/doc/License.html).

----

newLISP version 10.x.x for LINUX, FreeBSD, macOS Solaris and Windows
====================================================================


INTRODUCTION
------------

newLISP(tm) is LISP like scripting language for general programming
artificial intelligence and statistics. 

For more details about newLISP see the Users Manual and Reference in the
distribution file: 'doc/newlisp_manual.html' 

Code contributions, bug fixes, reports and comments are welcome. New
versions are available at:

    http://newlisp.org
      or
    http://newlisp.org/downloads
      or
    http://sourceforge.net/projects/newlisp

See the file doc/CHANGES in the source distribtion for changes over 
previous versions or

    http://newlisp.org/downloads/newLISP_xx_Release_Notes.html

New versions in development are available at:

    http://newlisp.org/downloads/development

Please contact me via email at: lutz@nuevatec.com.

Some of the information in this file is expanded in file newlisp-x.x.x/doc/INSTALL.


BUILD, CHECK and INSTALL
------------------------

Normally the correct makefile_xxxxx will be picked automatically just typing: 

    make

to discover the current platform and make for Linux, MacOSX, FreeBSD, OpenBSD,
NetBSD, Solaris/SunOS and Win32 (MinGW on MSYS). Sometimes the makefile_xxxxx
must be specified explicitely, e.g. when compiling for the RaspberryPi runnning
a sub-flavor of Linux and cross-compiled on a desktop system:

    make -f makefile_raspberrypi

Any other makefile_xxxxx can be picked the same way, but always read the
makefile_xxxxx first, sometimes it contains special instructions!

To test the compiled newlisp executable do:

    make check

or showing only success or failure:

    make testall

From a user account if the sudo command and admin password is available:

    sudo make install

If root permissions are not available:

    make install_home

home_install does not install the newLISP-GS Java based IDE, but installs
the supportig documentation.

see all flavors and platforms available for install:

    make help

will list all available platforms and flavors, newLISP can also be made as
a shared, dhynamic library: newlisp.so, newlisp.dylib or  newlisp.dll

To discover the platform in an extra step do:

    ./configure

    make

this discovers the platform and generates a makefile_build which
is used subsequently by make. As an alternative use:

    ./configure-alt

    make

this does a more elaborate analysys of options supported on a specific
platform and custom makes a makefile_build.

See the files doc/INSTALL, doc/LOCALIZATION and doc/FILES for more
detailed information.


MINIMUM INSTALL
---------------

Note that for a minumum install only the executable newlisp or newlisp.exe
in Win32 is necessary.

On Mac OS X or other UNIX copy  newlisp to /usr/local/bin or /usr/bin or ~/bin 
and give it executable permissions.


LICENSE
-------

newLISP and Nuevatec are trademarks of Lutz Mueller. Files in the newLISP
distribution are protected by the "GNU General Public License Version 3, June 2007". 
For the full text of this license see the accompanying file COPYING in the doc
directory, or the appendix in the file newlisp_manual.html. Documentation files
are protected by the "GNU Free Doumentation License Version 1.2, November 2002".
A copy of this license is also included in the file COPYING.

This and information about lcensing from other contributors to newLISP is
contained in the file COPYING in the source distribution.


MORE INFO AND CONTACT
---------------------

See in the doc/ directory of the source distribution

lutz@nuevatec.com


                                  +++

