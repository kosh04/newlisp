;; @module cgi.lsp
;; @description Basic CGI processing tools for GET and POST requests
;; @version v 2.2 - comments redone for automatic documentation
;; @version v 2.3 - a fix in CGI:url-translate when translating utf-8 strings
;; @version v 2.4 - check for empty string when aquiring HTTP_COOKIE environment
;; @version v 2.5 - cleanup put-page for 10.0
;; @version v 2.6 - help text corrections
;; @version v 2.72 - help text corrections
;; @version v 2.8 - check for and use CONTENT_LENGTH when reading POST data
;; @version v 2.9 - eliminate deprecated integer -> int
;; @version v 2.91 - minor code cleanup in CONTENT_LENGTH handling
;; @version v 3.0 - handle multipart POST data. Added by Unya, Oct 2012
;; @version v 3.2 - fixed for large POST data
;; @author Lutz Mueller, Unya 2002-2012
;;
;; This module defines basic CGI processing tools for processing
;; CGI GET and POST requests and cookies.
;;
;; Include this file at the beginning of each file performing CGI processing using
;; either one of the following lines:
;; <pre>
;; (load "/usr/share/newlisp/cgi.lsp")
;; ; or as a shorter alternative
;; (module "cgi.lsp")
;; </pre>
;; <center><h2>Overview</h2></center>
;; On loading 'cgi.lsp' will retrieve 'GET', 'POST' and cookie
;; parameters via standard input and the environment variables: 
;; 'QUERY_STRING' and 'HTTP_COOKIE'. These environment variables are set
;; by the webserver (tested with Apache 1.3). The webserver is receiving information
;; back from 'cgi.lsp' via std I/O channels.
;; 
;; After having loaded this file all parameters from either 'GET' or 'POST'
;; method are stored as an association list and in 'CGI:params'
;; and individual parameters can be accessed using 'CGI:get'.
;;
;; All cookies can be accessed in an association list 'CGI:cookies' and
;; are accessed similar to the 'GET' and 'PUT' parameters using 'CGI:get-cookie'.
;; A function 'CGI:set-cookie' is available for setting cookies.
;;
;; The function 'CGI:put-page' outputs a HTML page to the webserver after
;; processing newLISP source embedded in '&lt;%' and '%&gt;' tags.
;;
;; 'CGI:params' and 'CGI:cookies' contain the empty list '()' when no
;; parameters or cookies are present
;;
;; The function 'CGI:put-page' can be used to output web pages containing
;; newLISP source embedded in &lt;%, %&gt; tags. Inside these tags are newLISP
;; statements printing output/HTML to the webpage.

(context 'CGI)

;; @syntax (CGI:put-page <str-file-name>)
;; @param <str-file-name> The HTML file containing '&lt;%' and '%&gt;' tags.
;; @return The page output to standard out.
;; Processes an HTML page by evaluating newLISP source
;; embedded into the HTML text between '&lt;%' and '%&gt;' tags.
;; The newLISP source typically contains 'print' and 'println'
;; statements to output strings to standard out.
;;
;; @example
;; <html>
;; <body>
;; <% (set 'site "example.com") %>
;; <a href="http://<% (print site) %>"><% (print site) %></a>
;; </body>
;; </html>
;; 
;; - will output -
;; 
;; <html>
;; <body>
;; <a href="http://example.com">example.com</a>
;; </body>
;; </html>

(define (put-page file-name , page start end)
    (set 'page (read-file file-name))
    (set 'start (find "<%" page))
    (set 'end (find "%>" page))
    (while (and start end)
        (print (slice page 0 start))
        (eval-string (slice page (+ start 2) (- end start 2)) MAIN)
        (set 'page (slice page (+ end 2)))
        (set 'start (find "<%" page))
        (set 'end (find "%>" page)))
    (print page))

;; @syntax (CGI:url-translate <str-url-format>)
;; @param <str-url-format> The URL formatted string to translate.
;; @return An ASCII formatted string.
;; Translates all URL formatted characters to ASCII. Translates '+' into spaces 
;; and '%nn' hexdigits into characters. 'nn' is a 2-nibble hex number. 
;;
;; @example
;; (CGI:url-translate "What+time+is+it%3f")  => "What time is it?"

(define (url-translate str)
   (replace "+" str " ")
   (replace "%([0-9A-F][0-9A-F])" str (format "%c" (int (append "0x" $1))) 1))


; This is not an user function, but used internally.
;
; get-vars returns all parameter value pairs in an association list
; i.e.: ( ("name" "johndoe") ("password" "secret") )
; they can than be accessed using:
;     (assoc "name" params) => "johndoe"
; where params is the return value from get-vars
 
(define (get-vars input , var value var-value)
    (set 'vars (parse input "&"))
    (dolist (elmnt vars) 
	(if (find "=" elmnt) 
	    (begin
              (set 'var (first (parse elmnt "=")))
              (set 'value ((+ (find "=" elmnt) 1) elmnt)))
	    (begin
	      (set 'var elmnt)
	      (set 'value "")))
	(push (list var (url-translate value)) var-value))
    var-value) ; no necessary after v.9.9.5


; get QUERY_STRING parameters from GET method if present
;
(set 'params (env "QUERY_STRING"))
(if (not params) (set 'params ""))
(if params
	(set 'params (get-vars params)))

; get POST data if present, use CONTENT_LENGTH variable
; if available
(if (env "CONTENT_LENGTH")
    (when (= (env "REQUEST_METHOD") "POST")
        (set 'post-data "")
        (while (read (device) buffer (int (env "CONTENT_LENGTH")))
            (write post-data buffer))

        (let (boundary (when (regex "multipart/form-data; boundary=(.*)"
                (env "CONTENT_TYPE")) (append "--" $1)))
        ; handle multi part form data
          (if boundary
             (let (data (parse (url-translate post-data) boundary))
                (while data
                    (let (line (first data))
                        (setq data (rest data))
                        (when
                            (regex "Content-Disposition: form-data; name=\"(.*)\"\r\n\r\n(.*)$" line 4)
                                (push (list $1 (chop $2 2)) params) )
                    ) ) )
              (set 'params (get-vars post-data)))
		) )
    (begin
        (set 'inline (read-line))
        (when inline 
        (set 'params (get-vars inline)))
    )
)
 
(if (not params) (set 'params '()))


; get cookies
;
(if (and (env "HTTP_COOKIE") (not (empty? (env "HTTP_COOKIE"))))
    (dolist (elmnt (parse (env "HTTP_COOKIE") ";"))
		(set 'var (trim (first (parse elmnt "="))))
		(set 'value (slice elmnt (+ 1 (find "=" elmnt))))
		(push (list var value) cookies))
    (set 'cookies '()))

;; @syntax (CGI:set-cookie <str-var> <str-value> <str-domain> <str-path>)
;; @param <str-var> The cookie variable name as a string.
;; @param <str-value> The cookie value as a string.
;; @param <str-domain> The domain where to set the cookie.
;; @param <str-path> The path for the domain.
;; @return The string sent to standard out by 'CGI:set-cookie'.
;; This function should be called immedeately before
;; closing the header with '(print "Content-Type: text/html\r\n\r\n")',
;; which is typically the first statement in a CGI script written in
;; newLISP after the '(module "cgi.lsp")' statement.
;;
;; @example
;; (module "cgi.lsp")
;;
;; (CGI:set-cookie "password" "secret" "asite.com" "/somedir")
;; (print "Content-Type: text/html\r\n\r\n")
;; ...

(define (set-cookie var value domain path)
    (set 'value (string value))
    (print (format "Set-Cookie: %s=%s; domain=.%s; path=%s;\n" var value domain path)))


;; @syntax (CGI:get-cookie <str-key>)
;; @param <str-key> The string for the cookie variable name.
;; @return The string for the cookie value.
;; @example
;; (CGI:get-cookie "login")   =>  "somebody" 

(define (get-cookie keystr)
	(lookup keystr cookies) )

;; @syntax (CGI:get <str-key>)
;; @param The name of the 'GET' or 'POST' variable as a string.
;; @return The value string of the 'GET' or 'POST' variable.

(define (get keystr)
   (lookup keystr params))

;; @example
;; (CGI:get "city") => "San Francisco"


(context 'MAIN)

; eof ;
