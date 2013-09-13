#
# USAGE:
#
# make <option>
#
# to see a list of all options, enter 'make help'
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
# for customization options, like install location, 64-bit nerwlisp,
# newLISP as a library etc., see the file doc/INSTALL.txt
#
# Regular expressions are on all platforms Perl Compatible Regular Expresssions PCRE
# see http://www.pcre.org. PCRE can be localized to other languages than English
# by generating different character tables, see documentation at www.pcre.org
# and file LOCALIZATION for details
#

VERSION = 10.5.3
INT_VERSION = 10503

default: makefile_build
	make -f makefile_build

makefile_build:
	./configure

all: default

help:
	@echo "\nDo one of the following:"
	@echo "  make                 # auto-select one of the predefined makefiles and build newLISP"
	@echo "  make help            # display this help"
	@echo "  make install         # install on LINUX/UNIX in /usr/bin and /usr/share (need to be root)"
	@echo "  make uninstall       # uninstall on LINUX/UNIX from /usr/bin and /usr/share (need to be root)"
	@echo "  make install_home    # install on LINUX/UNIX in users home directory "
	@echo "  make uninstall_home  # uninstall on LINUX/UNIX from users home directory "
	@echo
	@echo "  make clean           # remove all *.o and .tar files etc. USE BETWEEN FLAVORS!"
	@echo "  make check           # run qa-dot, qa-net, qa-xml etc. test scripts"
	@echo "  make test            # same as 'make check' but less output"
	@echo "  make testall         # run an extended test suite with less output"
	@echo "  make version         # replace version number in several files after changing in Makefile"
	@echo "  make bench           # benchmark relative to 32-bit Mac OS 10.5 on MacMini 1.83Ghz"
	@echo "  make dist            # make a source distribution .tgz package "
	@echo "  make android_dist    # make a source package for Android NDK compilation"
	@echo "  make android_dist_utf8  # make a source package for Android NDK compilationi utf8"
	@echo
	@echo "Note! on some systems use gmake instead of make."
	@echo "Note! not all makefiles are listed in this help, specifically 64-bit versions."
	@echo " "
	@echo "Readline is for commandline editing support and requires libreadline and headerfiles."
	@echo "Not all makefiles contain libreadline support, but is easy to add (see other makefieles)."
	@echo "If there is no UTF-8 option for your OS, consult makefile_xxx."
	@echo " "
	@echo "For more customization options (exe dir, install dir, 64-biti, etc) see the file doc/INSTALL"

# make newlisp.exe and newlisp.dll on a MinGW, MSYS system
# also needs the installer NSYS installed
winall:
	make clean
	make -f makefile_mingw_ffi
	rm *.o
	make -f makefile_mingwdll_ffi
	rm *.o
	./newlisp qa-dot

# make newlisp.exe and newlisp.dll in UTF-8 flavor
winall_utf8:
	make clean
	make -f makefile_mingw_utf8_ffi
	rm *.o
	make -f makefile_mingwdll_utf8_ffi
	rm *.o
	./newlisp qa-dot
	tar czvf newlisp-win-utf8.tgz newlisp.exe newlisp.dll

# make a Windows intaller package
wings:
	make -f makefile_wings
	

# scripts for making UBUNTU linux packages

dpkg:
	make clean
	cp makefile_original_install makefile_install
	make -f makefile_linux_ffi
	cp util/description-pak .
	sudo checkinstall --nodoc --maintainer "lutz@nuevatec.com" --pkgrelease 1 --default
	rm description-pak
	mv *.deb ../Desktop

dpkg_utf8:
	make clean
	cp makefile_original_install makefile_install
	make -f makefile_linux_utf8_ffi
	cp util/description-pak .
	sudo checkinstall --nodoc --maintainer "lutz@nuevatec.com" --pkgrelease utf8 --default
	rm description-pak
	mv *.deb ../Desktop

# scripts for making Mac OS X disk image installers
# Note that since Mac OX X update 10.5.6 'PackageMaker -d -o'
# bombs out in the following script, but is fixed in 10.6.0
# Snow Leopard.
# one Mountain Lion use packagemaker productbuild and productsign
# see also: "Distributing Outside the Mac App Store" on http://developer.apple.com/
# linked from https://developer.apple.com/resources/developer-id/
dmg_ppc:
	make -f makefile_darwin_utf8_leopardPPC_ffi
	sudo rm -rf ../Package_contents
	make -f makefile_darwin_package
	/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker \
		-d ~/newlisp/OSX-package/newLISPpackage-project.pmdoc/ -o \
		~/newlisp/OSX-package/newLISP-image/newLISPpackage.pkg
	-rm ~/newlisp/OSX-package/newlisp-$(VERSION)-OSX-ppc.dmg
	hdiutil create -srcfolder ~/newlisp/OSX-package/newLISP-image \
		~/newlisp/OSX-package/newlisp-$(VERSION)-OSX-ppc.dmg

dmg_intel:
	make -f makefile_darwinLP64_utf8_ffi
	sudo rm -rf ../Package_contents
	make -f makefile_darwin_package
	/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker \
		-d ~/newlisp/OSX-package/newLISPpackage-project.pmdoc/ -o \
		~/newlisp/OSX-package/newLISP-image/newLISPpackage.pkg
	-rm ~/newlisp/OSX-package/newlisp-$(VERSION)-OSX-intel.dmg
	hdiutil create -srcfolder ~/newlisp/OSX-package/newLISP-image \
		~/newlisp/OSX-package/newlisp-$(VERSION)-OSX-intel.dmg

# this cleans up the distribution directory for a clean build from scratch

# this cleans the tree for a rebuild using the same configuration as before
clean:
	-rm -f *~ *.bak *.o *.obj *.map *.core core *.tgz guiserver/java/._* TEST newlisp-universal
	-rm -f guiserver/*.class doc/*~ util/*~ examples/*~ modules/*~
	-rm -f doc/*.bak util/*.bak examples/*.bak modules/*.bak
	-chmod 644 *.h *.c Makefile makefile*
	-chmod 755 configure configure-alt examples/*
	-chmod 644 doc/* modules/*.lsp examples/*.lsp examples/*.html
	-chmod 644 guiserver/*
	-chmod 755 guiserver/images
	-chmod 644 guiserver/images/*
	-chmod 755 guiserver/java
	-chmod 644 guiserver/java/*
	-rm -f makefile_build makefile_install config.h test-*

# run test scripts
check:
	./newlisp qa-dot
	./newlisp qa-specific-tests/qa-dictionary
	./newlisp qa-specific-tests/qa-xml
	./newlisp qa-specific-tests/qa-json
	./newlisp qa-specific-tests/qa-setsig
	./newlisp qa-specific-tests/qa-net
	./newlisp qa-specific-tests/qa-cilk
	./newlisp qa-specific-tests/qa-ref
	./newlisp qa-specific-tests/qa-message
	./newlisp qa-specific-tests/qa-win32dll
	./newlisp qa-specific-tests/qa-bigint 10000
	./newlisp qa-specific-tests/qa-bench

# old naming for check 
test: 
	make check | grep '>>>'

checkall:
	./newlisp qa-dot
	./newlisp qa-specific-tests/qa-dictionary
	./newlisp qa-specific-tests/qa-xml
	./newlisp qa-specific-tests/qa-json
	./newlisp qa-specific-tests/qa-setsig
	./newlisp qa-specific-tests/qa-net
	./newlisp qa-specific-tests/qa-net6
	./newlisp qa-specific-tests/qa-cilk
	./newlisp qa-specific-tests/qa-ref
	./newlisp qa-specific-tests/qa-message
	./newlisp qa-specific-tests/qa-win32dll
	./newlisp qa-specific-tests/qa-blockmemory
	./newlisp qa-specific-tests/qa-exception
	./newlisp qa-specific-tests/qa-float
	./newlisp qa-specific-tests/qa-foop
	./newlisp qa-specific-tests/qa-local-domain
	./newlisp qa-specific-tests/qa-inplace
	./newlisp qa-specific-tests/qa-utf16path
	./newlisp qa-specific-tests/qa-pipefork
	./newlisp qa-specific-tests/qa-ffi
	./newlisp qa-specific-tests/qa-libffi
	./newlisp qa-specific-tests/qa-bigint 10000
	./newlisp qa-specific-tests/qa-longnum
	./newlisp qa-specific-tests/qa-factorfibo 60
	./newlisp qa-specific-tests/qa-bench

testall: 
	make checkall | grep '>>>'

# benchmark
bench: 
	./newlisp qa-specific-tests/qa-bench

# install

# makefile_install normally is created by the configure script
# but when using 'make -f makefile_xxx' the file hasn't been
# created and is created with this dependency

makefile_install:
	cp makefile_original_install makefile_install

install: makefile_install
	-make -f makefile_install install

uninstall:
	-make -f makefile_install uninstall

install_home:
	-make -f makefile_install install_home

uninstall_home:
	-make -f makefile_install uninstall_home

# This make the main newlisp-x.x.x.tgz source distribuition package
dist: clean
	-mkdir newlisp-$(VERSION)
	-mkdir newlisp-$(VERSION)/guiserver
	-mkdir newlisp-$(VERSION)/guiserver/images
	-mkdir newlisp-$(VERSION)/guiserver/java
	-mkdir newlisp-$(VERSION)/modules
	-mkdir newlisp-$(VERSION)/examples
	-mkdir newlisp-$(VERSION)/doc
	-mkdir newlisp-$(VERSION)/util
	-mkdir newlisp-$(VERSION)/qa-specific-tests
	cp README newlisp-$(VERSION)
	cp nl*.c newlisp.c *.h pcre*.c newlisp-$(VERSION)
	cp win3*.* unix*.c newlisp-$(VERSION)
	cp Makefile configure* makefile* qa-dot qa-comma newlisp-$(VERSION)
	cp modules/* newlisp-$(VERSION)/modules
	cp examples/* newlisp-$(VERSION)/examples
	cp doc/* newlisp-$(VERSION)/doc
	cp util/* newlisp-$(VERSION)/util
	cp qa-specific-tests/* newlisp-$(VERSION)/qa-specific-tests
	cp -R guiserver/* newlisp-$(VERSION)/guiserver

	tar czvf newlisp-$(VERSION).tgz newlisp-$(VERSION)/*
	rm -rf newlisp-$(VERSION)
	mv newlisp-$(VERSION).tgz ..


# this makes a Android source pacage for compilation using the Android NDK
# may want to change APP_PLOTFORM spec to something different
android_dist_utf8:
	-mkdir newlisp-ndk-utf8-$(VERSION)
	-mkdir newlisp-ndk-utf8-$(VERSION)/jni
	-mkdir newlisp-ndk-utf8-$(VERSION)/libs
	-mkdir newlisp-ndk-utf8-$(VERSION)/libs/armeabi
	-mkdir newlisp-ndk-utf8-$(VERSION)/obj
	-mkdir newlisp-ndk-utf8-$(VERSION)/obj/local
	-mkdir newlisp-ndk-utf8-$(VERSION)/obj/local/armeabi
	cp nl*.c newlisp.c *.h pcre*.c newlisp-ndk-utf8-$(VERSION)/jni
	rm newlisp-ndk-utf8-$(VERSION)/jni/win-ffi.h
	cp doc/Android.html newlisp-ndk-utf8-$(VERSION)
	cp util/Android-utf8.mk newlisp-ndk-utf8-$(VERSION)/jni/Android.mk
	cp util/Application.mk newlisp-ndk-utf8-$(VERSION)/jni

	tar czvf newlisp-ndk-utf8-$(VERSION).tgz newlisp-ndk-utf8-$(VERSION)/*
	rm -rf newlisp-ndk-utf8-$(VERSION)
	mv newlisp-ndk-utf8-$(VERSION).tgz ..

android_dist:
	-mkdir newlisp-ndk-$(VERSION)
	-mkdir newlisp-ndk-$(VERSION)/jni
	-mkdir newlisp-ndk-$(VERSION)/libs
	-mkdir newlisp-ndk-$(VERSION)/libs/armeabi
	-mkdir newlisp-ndk-$(VERSION)/obj
	-mkdir newlisp-ndk-$(VERSION)/obj/local
	-mkdir newlisp-ndk-$(VERSION)/obj/local/armeabi
	cp nl*.c newlisp.c *.h pcre*.c newlisp-ndk-$(VERSION)/jni
	rm newlisp-ndk-$(VERSION)/jni/win-ffi.h
	rm newlisp-ndk-$(VERSION)/jni/nl-utf8.c 
	cp doc/Android.html newlisp-ndk-$(VERSION)
	cp util/Android.mk newlisp-ndk-$(VERSION)/jni
	cp util/Application.mk newlisp-ndk-$(VERSION)/jni

	tar czvf newlisp-ndk-$(VERSION).tgz newlisp-ndk-$(VERSION)/*
	rm -rf newlisp-ndk-$(VERSION)
	mv newlisp-ndk-$(VERSION).tgz ..


# this changes to the current version number in several files
#
# before doing a 'make version' the VERSION variable at the beginning
# of this file has to be changed to the new number
#
version:
	sed -i.bak -E 's/int version = .+;/int version = $(INT_VERSION);/' newlisp.c
	sed -i.bak -E 's/newLISP v.[[:digit:]]+.[[:digit:]]+.[[:digit:]]+(-dev)? /newLISP v.$(VERSION) /' newlisp.c
	sed -i.bak -E 's/newLISP\/[[:digit:]]+.[[:digit:]]+.[[:digit:]]+(-dev)?/newLISP\/$(VERSION)/' nl-web.c
	sed -i.bak -E 's/newLISP v.+ Manual/newLISP v.$(VERSION) Manual/' doc/newlisp_manual.html
	sed -i.bak -E 's/Reference v.+<\/h2>/Reference v.$(VERSION)<\/h2>/' doc/newlisp_manual.html
	sed -i.bak -E 's/newlisp-.....-win/newlisp-$(INT_VERSION)-win/' guiserver/newlisp-gs.nsi
	sed -i.bak -E 's/and newLISP .+ on /and newLISP $(VERSION) on /' guiserver/newlisp-gs.nsi
	sed -i.bak -E 's/VERSION=.+/VERSION=$(VERSION)/' configure-alt
	sed -i.bak -E 's/VERSION=.+/VERSION=$(VERSION)/' makefile_original_install 

# Prepare the manual file for PDF conversion, by replaceing all <span class="function"></span>
# with <font color="#DD0000"></font> in the syntax statements and replacing &rarr; (one line
# arrow with &rArr; (double line arrow). This is necessary when using OpenOffcice PDF conversion 
#
preparepdf:
	util/preparepdf doc/newlisp_manual.html doc/newlisp_manual_preparepdf.html

# end of file
