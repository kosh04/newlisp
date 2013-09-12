;; @module smtp.lsp 
;; @description Send mail using SMTP protocol
;; @version 2.0 - March 2008, Cormullion added AUTH PLAIN authentication 
;; @version 2.1 - changes for 10.0
;; @version 2.2 - doc changes
;; @version 2.3 - fix in mail-send-body, thanks to Alessandro
;; @version 2.31 - removed spurious apostrophe
;; @author Lutz Mueller 2001-2010, Cormullion 2008
;; <h2>Routines for sending mail</h2> 
;; This module implements routines to communicate with a SMTP mail server 
;; for sending email. To use this module include the following 'load' statement 
;; at the beginning of the program file: 
;; <pre> 
;; (load "/usr/share/newlisp/modules/smtp.lsp") 
;; ; or shorter
;; (module "smtp.lsp")
;; </pre> 
;; To see debugging information: <br><br>
;; <tt>(set 'debug-flag true)</tt>

(context 'SMTP) 

(set 'debug-flag nil) 

;; @syntax (SMTP:send-mail <str-from> <str-to> <str-subject> <str-message> <str-server>i [<str-usr> str-pass>]])
;; @param <str-from> The email address of the sender.
;; @param <str-to> The email address of the recipient.
;; @param <str-subject> The subject line of the email.
;; @param <str-message> The message part of the email.
;; @param <str-server> The address of the SMTP server.
;; @param <str-user> Optional user name for authentication.
;; @param <str-pass> Optional password for authentication.
;; @return On success 'true', on failure 'nil'.
;; In case the function fails returning 'nil', the function
;; 'SMTP:get-error-text' can be used to receive the error text.
;;
;; @example 
;;(SMTP:send-mail "jdoe@asite.com" "somebody@isp.com" "Greetings" 
;;   "How are you today? - john doe -" "smtp.asite.com" "jdoe" "secret") 

;; This logs in to the server, tries to authenticate using the username 'jdoe' and password 'secret' (if supplied), 
;; and sends an email with the format: 
;; <pre> 
;;  From:    jdoe@asite.com 
;;  To:      somebody@isp.com 
;;  Subject: Greetings 
;;  Message: How are you today? - John Doe - 
;; </pre> 


(context 'SMTP) 

(set 'debug-flag nil) 

(define (send-mail mail-from mail-to mail-subject mail-body SMTP-server (user-name "") (password "")) 
    (and 
        (set 'from-hostname (nth 1 (parse mail-from "@"))) 
        (set 'socket (net-connect SMTP-server 25)) 
        (confirm-request "2") 
        (net-send-get-result (append "HELO " from-hostname) "2") 
        (unless (and (empty?  user-name) (empty? password)) 
           (mail-authorize user-name password) true) 
        (net-send-get-result (append "MAIL FROM: <" mail-from ">") "2") 
        (net-send-get-result (append "RCPT TO: <" mail-to ">") "2") 
        (net-send-get-result "DATA" "3") 
        (mail-send-header) 
        (mail-send-body) 
        (confirm-request "2") 
        (net-send-get-result "QUIT" "2") 
        (or (net-close socket) true))) 

(define (confirm-request conf)
    (net-receive socket recvbuff 256 "\r\n")
    (if debug-flag (println recvbuff) true)
    ; Empty out pipe. According to SMTP spec, last line has valid code.
    ; added for 1.8 for newLISP 9.2.0
    (while (< 0 (net-peek socket))
        (net-receive socket recvbuff 256 "\r\n")
        (if debug-flag (println recvbuff)))
    (starts-with recvbuff conf))
 
(define (net-send-get-result str conf) 
   (set 'send-str (append str "\r\n")) 
   (if debug-flag (println "sent: " send-str)) 
   (net-send socket send-str) 
   (if conf (confirm-request conf) true)) 

(define (mail-authorize user-name password) 
   (net-send-get-result 
       (append "AUTH PLAIN " 
               (base64-enc (append "\000" user-name "\000" password))) "235")) 

(define (mail-send-header) 
    (net-send-get-result (append "TO: " mail-to)) 
    (net-send-get-result (append "FROM: " mail-from)) 
    (net-send-get-result (append "SUBJECT: " mail-subject)) 
    (net-send-get-result (append "X-Mailer: newLISP v." (string (nth -2 (sys-info)))))) 

(define (mail-send-body ) 
    (net-send-get-result "") 
    (dolist (lne (parse mail-body "\r\n")) 
        (if (starts-with lne ".") 
            (net-send-get-result (append "." lne)) 
            (net-send-get-result lne))) 
    (net-send-get-result ".")) 

(define (get-error-text) 
    recvbuff) 

(context 'MAIN) 

;  test

; (set 'SMTP:debug-flag true) 

; (SMTP:send-mail 
;  "from@example.com" 
;  "to@example.com" 
;  "title" 
;  "body" 
;  "smtp.example.com" 
;  "user.name" 
;  "password")) 

; eof
