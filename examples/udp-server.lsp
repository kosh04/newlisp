#!/usr/bin/newlisp

; demo server for non-blocking UDP communications
;
; start this program then start the client udp-client.lsp
;
; note, that net-listen in UDP mode only binds the socket
; to the local address, it does not 'listen' as in TCP/IP
; v.1.0

(set 'socket (net-listen 10001 "localhost" "udp"))
(if socket (println "server listening on port " 10001)
           (println (net-error)))
(while (not (net-error))
	(set 'msg (net-receive-from socket 255))
	(println "->" msg)
        (net-send-to (nth 1 msg) (nth 2 msg) (upper-case (first msg)) socket))

(exit)

;; eof

