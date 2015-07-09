To generate HTML documentation for the modules in this directory, simply
execute the following statement inside the newlisp-x.x.x/modules directory:

   newlispdoc -s -d *.lsp

or on Windows

   newlisp newlispdoc -s -d *.lsp

This will genereate an index page index.html and one html file each
for each module of the form name.lsp.html, where name is the name of the module.
For additional conversion options see newlisp-x.x.x/doc/newLISPdoc.html .

The newlispdoc utility can be found in newlisp-x.x.x/util/newlispdoc .

Documentation on how to format newLISP source files for this program can be found 
in newlisp-x.x.x/doc/newLISPdoc.html or in /usr/share/doc/newlisp/newLISPdoc.html.


