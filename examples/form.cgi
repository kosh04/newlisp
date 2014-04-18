#!/usr/bin/env newlisp
#
# Demo of CGI 1.1 interface 
#
# version 1.2  changed for newlisp v.10.0
# version 1.2a comments added
#
# get form data from CGI STDIN parse and
# write back to the client browser
#
# 
#


(print "Content-Type: text/html\r\n\r\n")
(println "<h3>Post or Get Variables</h3>\n")

;; Try to read POST data. For an alternative
;; method using CONTENT_LENGTH and CONTEN_TYPE
;; see the module cgi.lsp from the distribution
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
(dolist (e (env)) (print (e 0) "=" (e 1) "<br>"))
(println)

(exit)
