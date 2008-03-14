;; @module smtp.lsp
;; @description Routines for sending mail using SMPT protocol
;; @version 1.8 - comments redone for automatic documentation
;; @author Lutz Mueller 2001
;;
;; <h2>Routines for sending mail</h2>
;; This module implements routines to communicate with a SMTP mail server
;; for sending email. To use this module include the following 'load' statement
;; at the beginning of the program file:
;; <pre>
;; (load "/usr/share/newlisp/modules/smtp.lsp")
;; </pre>

;; @syntax (SMTP:send-mail <str-from> <str-to> <str-subject> <str-message> <str-server>)
;; @param <str-from> The email address of the sender.
;; @param <str-to> The email address of the recipient.
;; @param <str-subject> The subject line of the email.
;; @param <str-message> The message part of the email.
;; @param <str-server> The address of the SMTP server.
;; @return On success 'true', on failure 'nil'.
;; In case the function fails returning 'nil', the function
;; 'SMTP:get-error-text' can be used to receive the error text.
;;
;; @example
;;
;; (SMTP:send-mail "jdoe@asite.com" "somebody@isp.com" "Greetings"
;;                   "How are you today? - john doe -" "smtp.asite.com")

;; Will send mail:
;; <pre>
;;  from address: jdoe@asite.com
;;    to address: somebody@isp.com
;;  subject line: Greetings
;;  message body: Hoe are you today? - john doe-
;;     smtp host: smtp.asite.com
;; </pre>

;; @syntax (SMTP:get-error-text)
;; @return The text of the last error occured.

(context 'SMTP)

(set 'debug-flag nil)

; this is the main function to use
;
;  USAGE:
;

(define (send-mail mail-from mail-to mail-subject mail-body SMTP-server)
    (and
        (set 'from-hostname (nth 1 (parse mail-from "@")))
        (set 'socket (net-connect SMTP-server 25))
        (confirm-request "2")
        (net-send-get-result (append "HELO " from-hostname) "2")
        (net-send-get-result (append "MAIL FROM: <" mail-from ">") "2")
        (net-send-get-result (append "RCPT TO: <" mail-to ">") "2")
        (net-send-get-result "DATA" "3")
        (mail-send-header)
        (mail-send-body)
        (confirm-request "2")
        (net-send-get-result "QUIT" "2")
        (or (net-close socket) true)))

(define (confirm-request conf)
    (net-receive socket 'recvbuff 256 "\r\n")
    (if debug-flag (println recvbuff) true)
	; Empty out pipe. According to SMTP spec, last line has valid code.
	; added for 1.8 for newLISP 9.2.0
	(while (< 0 (net-peek socket))
		(net-receive socket 'recvbuff 256 "\r\n")
		(if debug-flag (println recvbuff)))
	(starts-with recvbuff conf))
  
(define (net-send-get-result str conf)
   (set 'send-str (append str "\r\n"))
   (if debug-flag (println "sent: " send-str)) 
   (net-send socket 'send-str)
   (if conf (confirm-request conf) true))

(define (mail-send-header)
    (net-send-get-result (append "TO: " mail-to))
    (net-send-get-result (append "FROM: " mail-from))
    (net-send-get-result (append "SUBJECT: " mail-subject))
    (net-send-get-result (append "X-Mailer: newLISP v." (string (nth -2 (sys-info)))))) 

(define (mail-send-body )
    (net-send-get-result "")
    (dolist (lne (parse mail-body "\r\n")) 
        (if (= lne ".") 
            (net-send-get-result "..")
            (net-send-get-result lne)))
    (net-send-get-result "."))

(define (get-error-text)
    recvbuff)

(context 'MAIN)

; eof
