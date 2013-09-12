# this make file is only used during development
# the main makefile in the newlisp-x.x.x/ will
# normally peform the installation below

all:
	javac -Xlint java/*.java
	mv java/*.class .
	jar cmf Manifest guiserver.jar *.class images/*.png
	rm *.class

install:
	-install -d  /usr/share/newlisp/guiserver
	-install -d  /usr/share/doc/newlisp/guiserver
	-install -m 644 guiserver.jar /usr/share/newlisp/
	-install -m 644 guiserver.lsp /usr/share/newlisp/
	-install -m 644 index.html /usr/share/doc/newlisp/guiserver/
	-install -m 644 guiserver.lsp.html /usr/share/doc/newlisp/guiserver/
	-install -m 755 newlisp-edit.lsp /usr/bin/newlisp-edit
