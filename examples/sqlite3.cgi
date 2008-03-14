#!/usr/bin/newlisp

;; this is a quick and dirty script for talking to your SQLite3 database
;; on your webserver
;;
;; sqlite3.cgi - version 1.0 - initial release
;;
;; requirements:
;;
;; (1) A 'libsqlite3.so' or 'sqlite3.dll' library for your server platform
;; edit the file 'sqlite.lsp' for the correct location.
;;
;; (2) The files 'cgi.lsp' and 'sqlite3.lsp', edit the (load ...) statements
;; below for the correct locations of these files.
;;
;;

(print "Content-type: text/html\n\n")

(load "cgi-bin/cgi.lsp")
(load "cgi-bin/sqlite3.lsp")

(set 'log-file "sqlite-log.txt") 

;; log access, log is only written, if log-file exists
;;
(set 'fle (open log-file "u"))
(if fle (begin
    (seek fle -1)
    (set 'ip-no (env "REMOTE_ADDR"))
    (unless (starts-with ip-no "99")
      (write-line (string 
           (date (+ (apply date-value (now)) (* 7 60 60)))
           " - " ip-no " - " (env "HTTP_USER_AGENT")) fle))
    (close fle)))


(if (empty? CGI:params)
    (set 'mode "input")
    (set 'mode (CGI:get "mode")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-table data)
    (println {<table border=1>})
    (dolist (line data)
        (println {<tr>})
        (dolist (cell line)
            (println {<td>} cell {</td>}))
        (println {</tr>}) )
    (println {<table>}))



;; get input sql ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (= mode "input")
    (begin
       (println 
[text]

<br>
<center>
<form action="/sqlte.cgi" method="POST">
Enter Name of Database:&nbsp;&nbsp;<input type="text" name="database">
<br>
<p>Enter SQL</p>
<textarea name="sql-str" rows=10 cols=80></textarea>
<br>
<input type="submit" value="Go">
<input type="hidden" name="mode" value="sql">
</center>

[/text]
)))

(if (= mode "sql")
    (begin
        (println {<br>})
	(set 'database (CGI:get "database"))
	(set 'sql-str (CGI:get "sql-str"))
        (sql3:open database)
	(set 'data (sql3:sql sql-str))
	(if (not data) 
            (println (sql3:error) {<br>}) 
            (if (list? data)
		(display-table data)
		(println data)))

        (println {<br>Hit the [back] button on your browser to got back<br>})))

(exit)

;; eof ;;
