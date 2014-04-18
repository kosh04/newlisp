#!/usr/bin/env newlisp

; Demo client for non-blocking UDP communications
;
; Start the server program udp-server.lsp first.
;
; Note, that net-listen in UDP mode only binds the socket
; to the local address, it does not 'listen' as in TCP/IP.
; v.1.0
; v.1.1 change net-receive for 10.0

(set 'socket (net-listen 10002 "" "udp"))
(if (not socket) (println (net-error)))
(while (not (net-error))
	(print "enter something -> ")
	(net-send-to  "127.0.0.1" 10001 (read-line) socket)
	(net-receive socket buff 255)
	(println "=> " buff))

(exit)

; eof

