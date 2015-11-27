;; @module pop3.lsp
;; @description POP3 mail retrieval routines
;; @version 2.0 - eliminated old net-send syntax
;; @version 2.1 - changes for 10.0
;; @version 2.3 - three fixes by winger 2012-08-29 (search: winger's fix)
;; @version 2.4 - changes references to /usr/ to /usr/local/
; Do not fully understand winger's 'net-receive-blank' , couldn't 'net-flush'
; be used instead? The problem seems to be trailing spaces after "+OK".
; Can somebody with access to a pop3 server verify?
;; @author Lutz Mueller et al., 2001, 2002, 2008, 2010, 2012
;;
;;
;; <h2>POP3 mail retrieval routines</h2>
;; Only the module 'pop3.lsp' is required, not other libraries need to be
;; present. Not all mailservers support all functions.
;;
;; To use the module put a 'load' statement at the top of your file:
;; <pre>
;; (load "/usr/local/share/newlisp/modules/pop3.lsp")
;; ; or shorter
;; (module "pop3")
;; </pre>
;;
;; <h2>Function overview</h2>
;; Load down all messages and put them in a directory 'messages/':
;; <pre>
;; (POP3:get-all-mail "user" "password" "pop.my-isp.com" "messages/")
;; </pre>
;; Load down only new messages:
;; <pre>
;; (POP3:get-new-mail "user" "password" "pop.my-isp.com" "messages/")
;; </pre>
;; Delete messages, which have not been read:
;; <pre>
;; (POP3:delete-old-mail "user" "password" "pop.my-isp.com")
;; </pre>
;; Delete all messages:
;; <pre>
;; (POP3:delete-all-mail "user" "password" "pop.my-isp.com")
;; </pre>
;; Get a list of status numbers '(<totalMessages>, <totalBytes>, <lastRead>)':
;; <pre>
;; (POP3:get-mail-status "user" "password" "pop.my-isp.com")
;; </pre>
;; Get error message for failed all/new/status function:
;; <pre>
;; (POP3:get-error-text)
;; </pre>
;; All functions return 'nil' on error and 'POP3:get-error-text' can be used to
;; retrieve the error text.
;;
;; The variable 'POP3:debug-flag' can be set to 'true' to display all of the
;; dialog with the pop2 mail server.

(context 'POP3)

(set 'debug-flag nil)

;; @syntax (POP3:get-all-mail <str-user> <str-password> <str-server> <str-dir>)
;; @param <str-user> The user ID.
;; @param <str-password> The password for the user ID.
;; @param <str-dir> The local directory for the retrieved mail.
;; @return On success 'true' else 'nil'.

(define (get-all-mail userName password pop3server mail-dir)
    (and
        (connect pop3server)
        (logon userName password)
        (set 'status (get-status))
        (set 'no-msgs (nth 2 status))
        (if (> no-msgs 0)
          (get-messages 1 no-msgs mail-dir)
          true)
        (log-off)))

;; @syntax (POP3:get-new-mail <str-user> <str-password> <str-server> <str-dir>)
;; @param <str-user> The user ID.
;; @param <str-password> The password for the user ID.
;; @param <str-dir> The local directory for the retrieved mail.
;; @return On success returns 'true' else 'nil'.
;; On failure use 'POP3:get-error-text' to retrieve the text of
;; the last error which occured.

(define (get-new-mail userName password pop3server mail-dir)
    (and
        (connect pop3server)
        (logon userName password)
        (set 'status (get-status true))
        (if (<= (first status) (nth 2 status))
            ; winger's fix1 "messages are counted from 1"
            (get-messages (++ (first status)) (nth 2 status) mail-dir)
            ; (get-messages (first status) (nth 2 status) mail-dir);
            true)
        (log-off)
    ) )

;; @syntax (POP3:get-mail-status <str-user> <str-password> <str-server>)
;; @param <str-user> The user ID.
;; @param <str-password> The password for the user ID.
;; @return A list of status information.
;; The list of status information returned contains the following items:
;; (<totalMessages>, <totalBytes>, <lastRead>)

(define (get-mail-status userName password pop3server)
    (and
        (connect pop3server)
        (logon userName password)
        (set 'status (get-status true))
        (log-off)
        status))

;; @syntax (POP3:delete-old-mail <str-user> <str-password> <str-server>)
;; @param <str-user> The user ID.
;; @param <str-password> The password for the user ID.
;; @return The number of messages left on the server.

(define (delete-old-mail userName password pop3server)
    (and
        (connect pop3server)
        (logon userName password)
        (set 'status (get-status true))
        (if (> (first status) 1)
            (for (msg 1 (- (first status) 1) ) (delete-message msg))
            true)
        (log-off)
        (first status)))

;; @syntax (POP3:delete-all-mail <str-user> <str-password> <str-server>)
;; @param <str-user> The user ID.
;; @param <str-password> The password for the user ID.
;; @return The number of the message last read.
(define (delete-all-mail userName password pop3server)
    (and
        (connect pop3server)
        (logon userName password)
        (set 'status (get-status))
        (if (> (last status) 0)
            (for (msg 1 (last status) ) (delete-message msg))
            true)
        (log-off)
        (last status)))

; receive request answer and verify
;
(define (net-confirm-request)
    (if (net-receive socket rcvbuff 512 "+OK")
        (begin
        (if debug-flag (println rcvbuff))
            (if (find "-ERR" rcvbuff)
                (finish rcvbuff)
                true))
        nil))

; winger's fix2 bypass " " of "+OK "
(define-macro (net-receive-blank int_socket sym-buffer max-bytes wait-string)
    (letex (int_socket (eval int_socket)
            sym-buffer sym-buffer
            max-bytes max-bytes)
        (if (and (net-receive int_socket sym-buffer max-bytes) (= " " sym-buffer))
            (net-receive int_socket sym-buffer max-bytes) )
    )
)

(define (net-flush)
    (if socket
        (while (> (net-peek socket) 0)
            (net-receive socket junk 256)
            (if debug-flag (println junk) )))
    true)

; connect to server
;
(define (connect server)
    (set 'socket (net-connect pop3server 110))
    (if (and debug-flag socket) (println "connected on: " socket) )
    (if (and socket (net-confirm-request))
        (net-flush)
        (finish "could not connect")))

;
(define (logon userName password)
    (and
        (set 'sndbuff (append "USER " userName "\r\n"))
        (net-send socket sndbuff)
        (if debug-flag (println "sent: " sndbuff) true)
        (net-confirm-request)
        (net-flush)
        (set 'sndbuff (append "PASS " password "\r\n"))
        (net-send socket sndbuff)
        (if debug-flag (println "sent: " sndbuff) true)
        (net-confirm-request)
        (net-flush)
        (if debug-flag (println "logon successful") true)))


; get status and last read
;
(define (get-status last-flag)
    (and
        (set 'sndbuff "STAT\r\n")
        (net-send socket sndbuff)
        (if debug-flag (println "sent: " sndbuff) true)
        (net-confirm-request)
        ; (net-receive socket status 256) ; old in 2.1 (10.4.3)
        (net-receive-blank socket status 256) ; new in 2.3 (10.4.4)
        (if debug-flag (println "status: " status) true)
        (net-flush)
        (if last-flag
            (begin
                (set 'sndbuff "LAST\r\n")
                (net-send socket sndbuff)
                (if debug-flag (println "sent: " sndbuff) true)
                (net-confirm-request)
                ; (net-receive socket last-read 256) ; old
                (net-receive-blank socket last-read 256) ; new
                (if debug-flag (println "last read: " last-read) true)
                (net-flush))
            (set 'last-read "0"))
        (set 'result (list (int (first (parse status)))))
        (if debug-flag (println "parsed status: " result) true)
        (push (int (nth 1 (parse status))) result)
        (push (int (first (parse last-read))) result)
        result)) ; not necessary starting 9.9.5 because push returns the list


; get a message
;
(define (retrieve-message , message)
    (set 'finished nil)
    (set 'message "")
    (while (not finished)
        (net-receive socket rcvbuff 16384)
        (set 'message (append message rcvbuff))
        (if (find "\r\n.\r\n" message) (set 'finished true)))
    (if debug-flag (println "received message") true)
    message)


; get all messages
;
; v 1.4: modified file name generation to improve uniqueness. (CaveGuy)
;        file name now created using last SMTP or ESMTP ID from header.
; v 1.5: changed file type to ".pop3" to reflect the context that created it.
;        (get-messages now forces the directory, if it does not exsist.
; v 1.6: make sure directory? doesn't have trailing slash in arg
;
(define (get-messages from to mail-dir)
   (if (ends-with mail-dir "/") (set 'mail-dir (chop mail-dir)))
   (if (if (not (directory? mail-dir)) (make-dir mail-dir) true)
       (begin
          (set 'mail-dir (append mail-dir "/")) 
          (for (msg from to)
               (if debug-flag (println "getting message " msg) true)
               (set 'sndbuff (append "RETR " (string msg) "\r\n"))
               (net-send socket sndbuff)
               (if debug-flag (println "sent: " sndbuff) true)
               (set 'message (retrieve-message))
               (if debug-flag (println (slice message 1 200)) true)
               (set 'istr (get-message-id message))
               (set 'istr (append mail-dir "ME-" istr))
               (if debug-flag (println "saving " istr) true)
               (write-file istr message)
               (if (not (rename-file istr (append istr ".pop3")))
               (delete-file istr)))))
    true) ; other parts of pop3 rely on 'true' return

; delete messages
;
(define (delete-message msg)
    (and
        (set 'sndbuff (append "DELE " (string msg) "\r\n"))
        (net-send socket sndbuff)
        (if debug-flag (println "sent: " sndbuff) true)
        (net-confirm-request)))

; get-message-date was
; changed to get-message-id
; v 1.4: CaveGuy

(define (get-message-id message)
    (set 'ipos (+ (find "id <| id |\tid " message 1) 5)
         ; winger's fix3 delete char '>'
         'iend (-- (find "@|;|\n|\r| |\t" (slice message ipos) 1)))
         ; 'iend (find "@|;|\n|\r| |\t" (slice message ipos) 1));
    (if debug-flag
    (print "Message ID: " (slice message ipos iend) "\n"))
    (set 'istr (slice message ipos iend)) )


; log off
;
(define (log-off)
    (set 'sndbuff "QUIT\r\n")
    (net-send socket sndbuff)
    (if debug-flag (println "sent: " sndbuff) true)
    (net-receive socket rcvbuff 256)
    (if debug-flag (println rcvbuff) true)
    true)

; report error and finish
;
(define (finish message)
    (if (ends-with message "+OK")
      (set 'message (chop message 3)))
    ;(print "<h3>" message "</h3>")
    (set 'mail-error-text message)
    (if debug-flag (println "ERROR: " message) true)
    (if socket (net-flush))
    (if socket (log-off))
    nil)

;; @syntax (POP3:get-error-text)
;; @return The text of the last error occurred.

(define (get-error-text) mail-error-text)

(context 'MAIN)


; test
;(if (not(POP3:get-all-mail "user" "password" "my-isp.com" "mail"))
;    (print (POP3:get-error-text)) true)
;(exit)
