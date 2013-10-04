#!/bin/sh

# this file is part of the OSX install image

sudo rm /usr/bin/newlisp
sudo rm /usr/bin/newlisp-edit
sudo rm /usr/bin/newlispdoc
sudo rm /usr/share/man/man1/newlispdoc.1
sudo rm /usr/share/man/man1/newlisp.1
sudo rm -rf /usr/share/newlisp/
sudo rm -rf /usr/share/doc/newlisp/
sudo rm -rf /Applications/newLISP-GS.app
sudo rm -rf /Library/Receipts/newLISPpackage.pkg

echo 'newLISP and newLISP-GS uninstalled, quit Terminal'
