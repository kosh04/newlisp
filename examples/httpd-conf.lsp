; httpd-conf.lsp
;;
; filter and translate HTTP request for newLISP
;; -c or -http server modes
;; reject query commands using CGI with .exe files

(command-event (fn (s)
    (let (request s)
		(when (find "?" s) ; is this a query
			(set 'request (first (parse s "?")))
			; discover illegal extension in queries
			(when (ends-with request ".exe")
				(set 'request "GET /errorpage.html")) )
		request)
))

; eof
