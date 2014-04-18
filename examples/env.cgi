#!/usr/bin/env newlisp
#

;;
;; prints the environment
;;
(print "Content-Type: text/html\r\n\r\n")
(print "<html>")

(print [text]
<A HREF="http://newlisp.org">Home</A>&nbsp;|
<A HREF="http:syntax.cgi?environment.txt">Source</A> 
[/text])

(println "<h2>" (date) "</h2>")
(println "<table border=1>")
(dolist (e (sort (env)))
  (if (and (!= (e 0) "HTTP_COOKIE") (!= (e 0) "UNIQUE_ID"))
      (println "<tr><td>" (e 0) "</td><td>" (e 1) "</td></tr>\n")
    )
)
(print "<h4>CGI by newLISP v" (sys-info -2)" on " ostype "<h4>")
(println "</html>")
(exit)

