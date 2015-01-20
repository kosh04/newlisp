#!/usr/bin/env newlisp

; demo server for non-blocking UDP communications
;
; Start this program then start the client udp-client.lsp
;
; Note, that net-listen in UDP mode only binds the socket
; to the local address, it does not 'listen' as in TCP/IP.
;
; On Windows the received string for the remote host also contains
; the port separated by a colon and must parsed out. On UNIX this
; is not necessary.
; v.1.0
; v.1.1 made it work for Windows parseing out the host label


(set 'socket (net-listen 10001 "localhost" "udp"))
(if socket (println "server listening on port " 10001)
           (println (net-error)))
(while (not (net-error))
	(set 'msg (net-receive-from socket 255))
	(println "->" msg)
	(net-send-to 
		(first (parse (nth 1 msg) ":")) (nth 2 msg) (upper-case (first msg)) socket))
(exit)
;; eof

