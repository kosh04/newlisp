#!/usr/bin/newlisp
#
# Demo of CGI 1.1 interface 
#
# version 1.1
#
# get form data from CGI STDIN parse and
# write back to the client browser
#
# 
#


(print "Content-type: text/html\r\n\r\n")
(println "<h3>Post or Get Variables</h3>\n")

(set 'input (read-line))
(if (not input) (set 'input (env "QUERY_STRING")))
(if input (begin
    (set 'vars (parse input "&"))
    (dolist (elmnt vars) 
	(replace "+" elmnt " ")
	(println elmnt"<br>"))
    (println "<p>")))

#
# command line arguments
(print "<h3>Command line</h3>\n" (main-args) "<p>\n")

#
# print environment variables
(println "<h3>Environment Variables</h3>")
(dolist (e (env)) (print e "<br>"))
(println "<br>")
(println "CGI by newLISP v." (sys-info -2))
(exit)
