# httpd.conf - sample file
#
# The path-name variable contains the path-name from the HTTP request
# query contains the query string. The function does not need to handle
# query, but must return a valid path-name string. If anything else
# than a string is returned, newlisp server ignores http-conf.
#
# In the following example all path-name trying to access unauthorized
# files causes the path-name returned asan access-denied.html error page.
# All path-name ending with the name of an existing directory on the
# server but wihtout the trailing "/" character generate a response
# with a "Location:" header spcifying the URL correctly. This will
# cause the client to reissue the request.
# 
# Using httpd.conf (a different filename can be chosen) newLISP's HTTP
# mode can be completely customized.
#
# EXAMPLE USAGE:
#    newlisp httpd.conf -http -d 8080 -w /usr/home/www
#
 
(define (httpd-conf path-name query)
	(if 
		;; check for unauthorized files
		(or
			(ends-with path-name ".exe")
			(starts-with path-name "/") 
			(find ".." path-name) 
		)
		(access-denied-error path-name)

		;; send back 'Location:' header on directories missing trailing '/'
		(and (directory? path-name) (not (ends-with path-name "/")))	
		(begin
			(read-headers)
			(print "HTTP/1.0 301 OK\r\n")
			(print (format "Server: newLISP v.%d (%s)\r\n" (sys-info -1) ostype))
			(print "Location: " path-name "/\r\n")
			(print "Content-type: text/html\r\n\r\n")
			nil) ; must return nil to finish processing

		;; else just return the original path-name
		true
		path-name
	))

(define (read-headers)
	(local (header)
		(while (!= (set 'header (read-line)) "")
			; add more if required
			(if (starts-with header "Host:" 0) (env "HTTP_HOST" (5 header)))
			(if (starts-with header "User-Agent:" 0) (env "HTTP_USER_AGENT" (11 header)))
		)))

(define (access-denied-error)
	(print "HTTP/1.0 200 OK\r\n")
	(print (format "Server: newLISP v.%d (%s)\r\n" (sys-info -1) ostype))
	(print "Content-type: text/html\r\n\r\n")
	(println "ERR:400 Access to " path-name " not allowed.\r\n")
	nil)

;; eof ;;
