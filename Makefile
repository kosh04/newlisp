#
# USAGE:
#
# make <option>
#
# to see a list of all options, enter 'make' without any options
#
# Note! on some systems do 'gmake' instead of 'make' (most BSD)
#
# for 'make install' you have to login as 'root' else do 'make install_home'
# note that 'make install_home' will not install guiserver files which
# must be in /usr/share/newlisp in MacOX X and UNIX machines
#
# to make the distribution archive:  'make dist'
#
# to clean up (delete .o *~ core etc.):  'make clean'
#
#
# Compile flags used:
#
# NANOSLEEP enables capability to return time in milli secs in 'time'
# READLINE enables commandline editing and history, requires readline lib
# NOIMPPORT disables the 'import' shared library import primitive
#
# Regular expressions now on all platforms Perl Compatible Regular Expresssions PCRE
# see http://www.pcre.org. PCRE can be localized to other languages than English
# by generating different character tables, see documentation at www.pcre.org
# and file LOCALIZATION for details
#

VERSION = 9.4.1
INT_VERSION = 9401
GUISERVER = /usr/share/newlisp/guiserver

default:
	./build

help:
	@echo "Do one of the following:"
	@echo
	@echo "  make linux           # newlisp for LINUX (tested Debian & Fedora)"
	@echo "  make linux_utf8      # newlisp for LINUX UTF-8"
	@echo "  make linux_readline  # newlisp for LINUX with readline support"
	@echo "  make linux_debian    # newlisp for LINUX with readline support for debian"
	@echo "  make linux_utf8_readline  # newlisp for LINUX UTF-8 with readline support"
	@echo "  make linux_lib       # newlisp.so as shared library for LINUX"
	@echo "  make linux_lib_utf8  # newlisp.so as shared library for LINUX with UTF-8"
	@echo "  make linux64ILP32    # newlisp for LINUX 64 with 32-bit pointers / AMD64"
	@echo "  make linux64LP64     # newlisp for LINUX 64 with 64-bit pointers / AMD64"
	@echo "  make tru64           # newlisp for HP tru64 with 32 bit pointers - read doc/TRU64BUILD"
	@echo "  make bsd             # newlisp for FreeBSD and OpenBSD"
	@echo "  make netbsd          # newlisp for NetBSD (same as previous w/o readline)"
	@echo "  make bsd_lib         # newlisp.so as shared library for FreeBSD, OpenBSD, NetBSD"
	@echo "  make darwin          # newlisp for Mac OSX v.10.4 or later, readline support"
	@echo "  make darwin_utf8     # newlisp for Mac OSX v.10.4 or later, readline and UTF-8 support"
	@echo "  make darwin_lib      # newlisp for Mac OSX v.10.3 or later as shared library"
	@echo "  make universal       # newlisp for Mac OSX v.10.3 universal version ppc+intel"
	@echo "  make universal_utf8  # newlisp for Mac OSX v.10.3 universal version ppc+intel UTF-8"
	@echo "  make solaris         # newLISP for Sun SOLARIS (tested on Sparc)"
	@echo "  make solarisLP64     # newLISP for Sun SOLARIS 64-bit LP64 (tested on Sparc)"
	@echo "  make solaris_utf8    # newLISP for Sun SOLARIS UTF-8 (tested on Sparc)"
	@echo "  make opensolaris     # newLISP for SunOS OpenSolaris on i386 INtel/AMD CPU"
	@echo "  make true64          # newLISP for tru64 UNIX LP64 tested on Alpha CPU"
	@echo "  make mingw           # newlisp.exe for Win32 (MinGW compiler)"
	@echo "  make mingw_utf8      # newlisp.exe for Win32 UTF-8 (MinGW icompiler)"
	@echo "  make mingwdll        # newlisp.dll for Win32 (MinGW compiler)"
	@echo "  make mingwdll_utf8   # newlisp.dll for Win32 UTF-8 (MinGW compiler)"
	@echo "  make os2             # newlisp for OS/2 GCC 3.3.5 with libc061.dll"
	@echo 
	@echo "  make install         # install on LINUX/UNIX in /usr/bin and /usr/share (need to be root)"
	@echo "  make uninstall       # uninstall on LINUX/UNIX from /usr/bin and /usr/share (need to be root)"
	@echo "  make install_home    # install on LINUX/UNIX in users home directory "
	@echo "  make uninstall_home  # uninstall on LINUX/UNIX from users home directory "
	@echo
	@echo "  make clean           # remove all *.o and .tar files etc. USE BETWEEN FLAVORS!"
	@echo "  make test            # run qa-dot, qa-net and qa-xml test scripts"
	@echo
	@echo "Note! on some systems use gmake instead of make"
	@echo "readline is for commandline editing support and requires libreadline"
	@echo "only installed by default on BSDs (FreeBSD, NetBSD, OpenBSD, MacOS X/Darwin)"
	@echo "If there is no UTF-8 option for your OS, consult makefile_xxx"

linux:
	make -f makefile_linux

linux_utf8:
	make -f makefile_linux_utf8

linux_readline:
	make -f makefile_linux_readline

linux_debian:
	make -f makefile_debian

debian:
	make -f makefile_debian

linux_utf8_readline:
	make -f makefile_linux_utf8_readline

debian_utf8:
	make -f makefile_linux_utf8_readline

linux_lib:
	make -f makefile_linux_lib

linux_lib_utf8:
	make -f makefile_linux_lib_utf8

linux64ILP32:
	make -f makefile_linux64ILP32
	
linux64LP64:
	make -f makefile_linux64LP64

tru64:
	make -f makefile_tru64

bsd:
	make -f makefile_bsd

netbsd:
	make -f makefile_netbsd

bsd_lib:
	make -f makefile_bsd_lib

darwin_lib:
	make -f makefile_darwin_lib

universal:
	make -f makefile_universal

universal_utf8:
	make -f makefile_universal_utf8

darwin:
	make -f makefile_darwin
	
darwin_utf8:
	make -f makefile_darwin_utf8

solaris:
	make -f makefile_solaris

solarisLP64:
	make -f makefile_solarisLP64

solaris_utf8:
	make -f makefile_solaris_utf8

opensolaris:
	make -f makefile_opensolaris

mingw:
	make -f makefile_mingw

mingw_utf8:
	make -f makefile_mingw_utf8

mingwdll:
	make -f makefile_mingwdll

mingwdll_utf8:
	make -f makefile_mingwdll_utf8

os2: 
	make -f makefile_os2 
	
winall:
	make clean
	make -f makefile_mingw
	make clean
	make -f makefile_mingwdll
	make clean
	./newlisp qa-dot

winall_utf8:
	make clean
	make -f makefile_mingw_utf8
	make clean
	make -f makefile_mingwdll_utf8
	make clean
	./newlisp qa-dot


wings:
	make -f makefile_wings

# this cleans up the distribution directory for a clean build
clean:
	-rm *~ *.bak *.o *.obj *.map core *.tgz guiserver/java/._* TEST
	-rm guiserver/*.class doc/*~ util/*~ examples/*~ modules/*~
	-rm  doc/*.bak util/*.bak examples/*.bak modules/*.bak
	-chmod 644 *.h *.c *.lsp Makefile makefile*
	-chmod 755 build configure examples/*
	-chmod 644 doc/* modules/*.lsp examples/*.lsp examples/*.html
	-chmod 644 guiserver/*
	-chmod 755 guiserver/images
	-chmod 644 guiserver/images/*
	-chmod 755 guiserver/java
	-chmod 644 guiserver/java/*

# run test scripts
test:
	./newlisp qa-dot
	./newlisp qa-dictionary
	./newlisp qa-xml
	./newlisp qa-setsig
	./newlisp qa-net
	./newlisp qa-cilk

# directory definitions
datadir=$(DESTDIR)/usr/share
bindir=$(DESTDIR)/usr/bin
mandir=$(DESTDIR)/usr/share/man

GUISERVER = /usr/share/newlisp/guiserver

# this is the standard install in /usr/bin and usr/share
# which as to be done as 'root' with supersuser permissions
# for an install in your home directory use make install_home
#
# One-line description for distribution packages: 
# newLISP is a LISP like, general purpose scripting language. 
#
# Longer description for distribution packages: 
# newLISP is a scripting language for developing web applications and programs 
# in general and in the domains of artificial intelligence (AI) and statistics.

install:
	-install -d $(datadir)/newlisp
	-install -d $(datadir)/newlisp/modules
	-install -d $(datadir)/newlisp/util
	-install -d $(datadir)/doc/newlisp
	-install -m 755 newlisp $(bindir)/newlisp
	-install -m 644 examples/init.lsp.example $(datadir)/newlisp/init.lsp.example
	-install -m 755 util/newlispdoc $(bindir)/newlispdoc
	-install -m 644 util/syntax.cgi $(datadir)/newlisp/util/syntax.cgi
	-install -m 644 util/newlisp.vim $(datadir)/newlisp/util/newlisp.vim
	-install -m 644 util/nanorc $(datadir)/newlisp/util/nanorc
	-install -m 644 util/link.lsp $(datadir)/newlisp/util/link.lsp
	-install -m 644 util/httpd-conf.lsp $(datadir)/newlisp/util/httpd-conf.lsp
	-install -m 644 doc/COPYING $(datadir)/doc/newlisp/COPYING
	-install -m 644 doc/CREDITS $(datadir)/doc/newlisp/CREDITS
	-install -m 644 doc/newlisp_manual.html $(datadir)/doc/newlisp/newlisp_manual.html
	-install -m 644 doc/newlisp_index.html $(datadir)/doc/newlisp/newlisp_index.html
	-install -m 644 doc/manual_frame.html $(datadir)/doc/newlisp/manual_frame.html
	-install -m 644 doc/CodePatterns.html $(datadir)/doc/newlisp/CodePatterns.html
	-install -m 644 doc/newLISPdoc.html $(datadir)/doc/newlisp/newLISPdoc.html
	-install -m 644 doc/newLISP-9.4-Release.html $(datadir)/doc/newlisp/newLISP-9.4-Release.html
	-install -m 644 doc/newlisp.1 $(mandir)/man1/newlisp.1
	-install -m 644 doc/newlispdoc.1 $(mandir)/man1/newlispdoc.1
	-install -m 644 modules/cgi.lsp $(datadir)/newlisp/modules/cgi.lsp
	-install -m 644 modules/crypto.lsp $(datadir)/newlisp/modules/crypto.lsp
	-install -m 644 modules/ftp.lsp $(datadir)/newlisp/modules/ftp.lsp
	-install -m 644 modules/gmp.lsp $(datadir)/newlisp/modules/gmp.lsp
	-install -m 644 modules/infix.lsp $(datadir)/newlisp/modules/infix.lsp
	-install -m 644 modules/mysql5.lsp $(datadir)/newlisp/modules/mysql5.lsp
	-install -m 644 modules/mysql51.lsp $(datadir)/newlisp/modules/mysql51.lsp
	-install -m 644 modules/odbc.lsp $(datadir)/newlisp/modules/odbc.lsp
	-install -m 644 modules/pop3.lsp $(datadir)/newlisp/modules/pop3.lsp
	-install -m 644 modules/postscript.lsp $(datadir)/newlisp/modules/postscript.lsp
	-install -m 644 modules/smtp.lsp $(datadir)/newlisp/modules/smtp.lsp
	-install -m 644 modules/sqlite3.lsp $(datadir)/newlisp/modules/sqlite3.lsp
	-install -m 644 modules/stat.lsp $(datadir)/newlisp/modules/stat.lsp
	-install -m 644 modules/unix.lsp $(datadir)/newlisp/modules/unix.lsp
	-install -m 644 modules/xmlrpc-client.lsp $(datadir)/newlisp/modules/xmlrpc-client.lsp
	-install -m 644 modules/zlib.lsp $(datadir)/newlisp/modules/zlib.lsp
	# GUI-Server install
	-install -d $(datadir)/newlisp/guiserver
	-install -d $(datadir)/doc/newlisp/guiserver
	-install -m 755 guiserver/newlisp-edit.lsp $(bindir)/newlisp-edit
	-install -m 644 guiserver/guiserver.jar $(datadir)/newlisp/guiserver.jar
	-install -m 644 guiserver/guiserver.lsp $(datadir)/newlisp/guiserver.lsp
	-install -m 644 guiserver/images/newLISP128.png $(datadir)/newlisp/newLISP128.png
	-install -m 644 guiserver/COPYING $(datadir)/doc/newlisp/guiserver/COPYING
	-install -m 644 guiserver/index.html $(datadir)/doc/newlisp/guiserver/index.html
	-install -m 644 guiserver/guiserver.lsp.html $(datadir)/doc/newlisp/guiserver/guiserver.lsp.html
	-install -m 644 util/newlispdoc.css $(datadir)/doc/newlisp/guiserver/newlispdoc.css
	-install -m 644 guiserver/allfonts-demo.lsp $(datadir)/newlisp/guiserver/allfonts-demo.lsp
	-install -m 644 guiserver/animation-demo.lsp $(datadir)/newlisp/guiserver/animation-demo.lsp
	-install -m 644 guiserver/border-layout-demo.lsp $(datadir)/newlisp/guiserver/border-layout-demo.lsp
	-install -m 644 guiserver/button-demo.lsp $(datadir)/newlisp/guiserver/button-demo.lsp
	-install -m 644 guiserver/clipboard-demo.lsp $(datadir)/newlisp/guiserver/clipboard-demo.lsp
	-install -m 644 guiserver/cursor-demo.lsp $(datadir)/newlisp/guiserver/cursor-demo.lsp
	-install -m 644 guiserver/drag-demo.lsp $(datadir)/newlisp/guiserver/drag-demo.lsp
	-install -m 644 guiserver/font-demo.lsp $(datadir)/newlisp/guiserver/font-demo.lsp
	-install -m 644 guiserver/frameless-demo.lsp $(datadir)/newlisp/guiserver/frameless-demo.lsp
	-install -m 644 guiserver/html-demo.lsp $(datadir)/newlisp/guiserver/html-demo.lsp
	-install -m 644 guiserver/image-demo.lsp $(datadir)/newlisp/guiserver/image-demo.lsp
	-install -m 644 guiserver/midi-demo.lsp $(datadir)/newlisp/guiserver/midi-demo.lsp
	-install -m 644 guiserver/midi2-demo.lsp $(datadir)/newlisp/guiserver/midi2-demo.lsp
	-install -m 644 guiserver/mouse-demo.lsp $(datadir)/newlisp/guiserver/mouse-demo.lsp
	-install -m 644 guiserver/move-resize-demo.lsp $(datadir)/newlisp/guiserver/move-resize-demo.lsp
	-install -m 644 guiserver/pinballs-demo.lsp $(datadir)/newlisp/guiserver/pinballs-demo.lsp
	-install -m 644 guiserver/properties-demo.lsp $(datadir)/newlisp/guiserver/properties-demo.lsp
	-install -m 644 guiserver/rotation-demo.lsp $(datadir)/newlisp/guiserver/rotation-demo.lsp
	-install -m 644 guiserver/shapes-demo.lsp $(datadir)/newlisp/guiserver/shapes-demo.lsp
	-install -m 644 guiserver/sound-demo.lsp $(datadir)/newlisp/guiserver/sound-demo.lsp
	-install -m 644 guiserver/stroke-demo.lsp $(datadir)/newlisp/guiserver/stroke-demo.lsp
	-install -m 644 guiserver/tabs-demo.lsp $(datadir)/newlisp/guiserver/tabs-demo.lsp
	-install -m 644 guiserver/textrot-demo.lsp $(datadir)/newlisp/guiserver/textrot-demo.lsp
	-install -m 644 guiserver/widgets-demo.lsp $(datadir)/newlisp/guiserver/widgets-demo.lsp
	-install -m 644 guiserver/word-count.lsp $(datadir)/newlisp/guiserver/word-count.lsp
	-install -m 644 guiserver/uppercase.lsp $(datadir)/newlisp/guiserver/uppercase.lsp


uninstall:
	-rm  $(bindir)/newlisp
	-rm  $(bindir)/newlispdoc
	-rm  $(bindir)/newlisp-edit
	-rm  -rf $(datadir)/newlisp
	-rm  -rf $(datadir)/doc/newlisp
	-rm  $(mandir)/man1/newlisp.1
	-rm  $(mandir)/man1/newlispdoc.1

# installs newLISP in home directory, but without guiserver files except
# documentation. To make guiserver run from ~/share/newlisp. The loading
# from guiserver.lsp at the beginning of a guiserver app and loading
# of guiserver.jar from inside of guiserver.lsp have to be changed.

install_home:
	-install -d $(HOME)/bin
	-install -d $(HOME)/share/newlisp
	-install -d $(HOME)/share/newlisp/modules
	-install -d $(HOME)/share/newlisp/util
	-install -d $(HOME)/share/doc/newlisp/
	-install -d $(HOME)/share/doc/newlisp/guiserver
	-install -d $(HOME)/share/man/man1
	-install -m 755 newlisp $(HOME)/bin/newlisp
	-install -m 644 examples/init.lsp.example $(HOME)/share/newlisp/init.lsp.example
	-install -m 755 util/newlispdoc $(HOME)/bin/newlispdoc
	-install -m 644 util/syntax.cgi $(HOME)/share/newlisp/util/syntax.cgi
	-install -m 644 util/newlisp.vim $(HOME)/share/newlisp/util/newlisp.vim
	-install -m 644 util/nanorc $(HOME)/share/newlisp/util/nanorc
	-install -m 644 util/link.lsp $(HOME)/share/newlisp/util/link.lsp
	-install -m 644 util/httpd-conf.lsp $(HOME)/share/newlisp/util/httpd-conf.lsp
	-install -m 644 guiserver/index.html $(HOME)/share/doc/newlisp/guiserver/index.html
	-install -m 644 guiserver/guiserver.lsp.html $(HOME)/share/doc/newlisp/guiserver/guiserver.lsp.html
	-install -m 644 doc/COPYING $(HOME)/share/doc/newlisp/COPYING
	-install -m 644 doc/COPYING $(HOME)/share/doc/newlisp/guiserver/COPYING
	-install -m 644 doc/CREDITS $(HOME)/share/doc/newlisp/CREDITS
	-install -m 644 doc/newlisp_manual.html $(HOME)/share/doc/newlisp/newlisp_manual.html
	-install -m 644 doc/newlisp_index.html $(HOME)/share/doc/newlisp/newlisp_index.html
	-install -m 644 doc/manual_frame.html $(HOME)/share/doc/newlisp/manual_frame.html
	-install -m 644 doc/CodePatterns.html $(HOME)/share/doc/newlisp/CodePatterns.html
	-install -m 644 doc/newLISPdoc.html $(HOME)/share/doc/newlisp/newLISPdoc.html
	-install -m 644 doc/newLISP-9.4-Release.html $(HOME)/share/doc/newlisp/newLISP-9.4-Release.html
	-install -m 644 doc/newlisp.1 $(HOME)/share/man/man1/newlisp.1
	-install -m 644 doc/newlispdoc.1 $(HOME)/share/man/man1/newlispdoc.1
	-install -m 644 modules/cgi.lsp $(HOME)/share/newlisp/modules/cgi.lsp
	-install -m 644 modules/crypto.lsp $(HOME)/share/newlisp/modules/crypto.lsp
	-install -m 644 modules/ftp.lsp $(HOME)/share/newlisp/modules/ftp.lsp
	-install -m 644 modules/gmp.lsp $(HOME)/share/newlisp/modules/gmp.lsp
	-install -m 644 modules/infix.lsp $(HOME)/share/newlisp/modules/infix.lsp
	-install -m 644 modules/mysql.lsp $(HOME)/share/newlisp/modules/mysql.lsp
	-install -m 644 modules/mysql5.lsp $(HOME)/share/newlisp/modules/mysql5.lsp
	-install -m 644 modules/mysql51.lsp $(HOME)/share/newlisp/modules/mysql51.lsp
	-install -m 644 modules/odbc.lsp $(HOME)/share/newlisp/modules/odbc.lsp
	-install -m 644 modules/pop3.lsp $(HOME)/share/newlisp/modules/pop3.lsp
	-install -m 644 modules/postscript.lsp $(HOME)/share/newlisp/modules/postscript.lsp
	-install -m 644 modules/smtp.lsp $(HOME)/share/newlisp/modules/smtp.lsp
	-install -m 644 modules/sqlite3.lsp $(HOME)/share/newlisp/modules/sqlite3.lsp
	-install -m 644 modules/stat.lsp $(HOME)/share/newlisp/modules/stat.lsp
	-install -m 644 modules/unix.lsp $(HOME)/share/newlisp/modules/unix.lsp
	-install -m 644 modules/xmlrpc-client.lsp $(HOME)/share/newlisp/modules/xmlrpc-client.lsp
	-install -m 644 modules/zlib.lsp $(HOME)/share/newlisp/modules/zlib.lsp


uninstall_home:
	-rm  -rf $(HOME)/share/newlisp
	-rm  -rf $(HOME)/share/doc/newlisp
	-rm  $(HOME)/share/man/man1/newlisp.1
	-rm $(HOME)/bin/newlisp
	-rm $(HOME)/bin/newlispdoc

# this makes the distribution newlisp-x.x.x.tgz from inside newlisp-x.x.x directory
# you shouldn't use this, but send me the changed files with your contribution/fixes 
# to lutz@nuevatec.com put the word: newlisp in the subject line
#
dist:
	-mkdir newlisp-$(VERSION)
	-mkdir newlisp-$(VERSION)/guiserver
	-mkdir newlisp-$(VERSION)/guiserver/images
	-mkdir newlisp-$(VERSION)/guiserver/java
	-mkdir newlisp-$(VERSION)/modules
	-mkdir newlisp-$(VERSION)/examples
	-mkdir newlisp-$(VERSION)/doc
	-mkdir newlisp-$(VERSION)/util
	cp README newlisp-$(VERSION)
	cp nl*.c newlisp.c *.h pcre*.c newlisp-$(VERSION)
	cp win3*.* unix*.c newlisp-$(VERSION)
	cp Makefile build configure makefile* qa* newlisp-$(VERSION)
	cp modules/* newlisp-$(VERSION)/modules
	cp examples/* newlisp-$(VERSION)/examples
	cp doc/* newlisp-$(VERSION)/doc
	cp util/* newlisp-$(VERSION)/util
	cp -R guiserver/* newlisp-$(VERSION)/guiserver

	tar czvf newlisp-$(VERSION).tgz newlisp-$(VERSION)/*
	rm -rf newlisp-$(VERSION)
	mv newlisp-$(VERSION).tgz ..

osx_package:	
	make -f makefile_osx_package

# this changes to the current version number in several files
#
# before doing a 'make version' the VERSION variable at the beginning
# of this file has to be changed to the new number
#
version:
	sed -i.bak -E 's/int version = .+;/int version = $(INT_VERSION);/' newlisp.c
	sed -i.bak -E 's/newLISP v.[[:digit:]]+.[[:digit:]]+.[[:digit:]]+ /newLISP v.$(VERSION) /' newlisp.c
	sed -i.bak -E 's/newLISP v.+ Manual/newLISP v.$(VERSION) Manual/' doc/newlisp_manual.html
	sed -i.bak -E 's/Reference v.+<\/h2>/Reference v.$(VERSION)<\/h2>/' doc/newlisp_manual.html
	sed -i.bak -E 's/newlisp-....-win/newlisp-$(INT_VERSION)-win/' guiserver/newlisp-gs.nsi
	sed -i.bak -E 's/and newLISP .+ on /and newLISP $(VERSION) on /' guiserver/newlisp-gs.nsi

# Prepare the manual file for PDF conversion, byt replaceing all <span class="function"></span>
# with <font color="#DD0000"></font> in the syntax statements and replacing &rarr; (one line
# arrow with &rArr; (double line arrow). This is necessary when using OpenOffcice PDF conversion 
#
preparepdf:
	util/preparepdf doc/newlisp_manual.html doc/newlisp_manual_preparepdf.html

# end of file
