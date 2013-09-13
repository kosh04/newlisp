;; @module smtpx.lsp
;; @description Send mail using SMTP protocol
;; @version 3.1 - "\'"real name\" <mail@domain.com>" now supported in <str-from> 
;;              -  added Date to (send-mail-header) using a gettimezone hack to avoid DATE_MISSING spam test
;;              -  Fixed "Bare lf's in body" error to make servers using RFC822 Spam filtering happy
;;        Note: -  My quick hack at (gettimezone) needs to be improved to work world-wide :)
;; @version 3.0 - Partial rewrite for Dragonfly. Addition attachments, custom port and proper utf8 encoding for subject/message/attachments
;; @version 2.3 - fix in mail-send-body, thanks to Alessandro
;; @version 2.2 - doc changes
;; @version 2.1 - changes for 10.0
;; @version 2.0 - March 2008, Cormullion added AUTH PLAIN authentication
;; @author Lutz Mueller 2001-2009, Cormullion 2008, Greg Slepak 2009-2010 
;;
(context 'SMTP)
;;
;; @syntax (SMTP:send-mail <str-from> <str-to> <str-subject> <str-message> [<str-server> [<str-usr> <str-pass> [<int-port>]]])
;; @param <str-from> The email address of the sender. "\"real name\"<mailname@domain.com>" support added in 3.x
;; @param <str-to> The email address of the recipient.
;; @param <str-subject> The subject line of the email.
;; @param <str-message> The message part of the email.
;; @param <str-server> The address of the SMTP server (default: "localhost")
;; @param <str-user> Optional user name for authentication.
;; @param <str-pass> Optional password for authentication.
;; @param <int-port> Optional port to communicate on (default: 25)
;; @return On success 'true', on failure 'nil'.
;; In case the function fails returning 'nil', the function
;; 'SMTP:get-error-text' can be used to receive the error text.
;;
;; @example
;; (SMTP:send-mail "jdoe@asite.com" "somebody@isp.com" "Greetings"
;;   "How are you today? - john doe -" "smtp.asite.com" "jdoe" "secret")
;;
;; This logs in to the server, tries to authenticate using the username 'jdoe' and password 'secret' (if supplied),
;; and sends an email with the format:
;;
;;  From:    jdoe@asite.com
;;  To:      somebody@isp.com
;;  Subject: Greetings
;;  Message: How are you today? - John Doe -
(define (send-mail mail-from mail-to mail-subject mail-body (SMTP-server "localhost") user-name password (port 25))
    (and
        (set 'from-hostname (nth 1 (parse mail-from "@")))
        (replace ">" from-hostname "")   ; 
        (set 'socket (net-connect SMTP-server port))
        (confirm-request "2")
        (net-send-get-result (string "HELO " from-hostname) "2")
        (if (or (null? user-name) (null? password)) 
           true (mail-authorize user-name password))
        (net-send-get-result (string "MAIL FROM: " mail-from ) "2")
        (net-send-get-result (string "RCPT TO: <" mail-to ">") "2")
        (net-send-get-result "DATA" "3")
        (mail-send-header)
        (mail-send-body)
        (confirm-request "2")
        (net-send-get-result "QUIT" "2")
        (or (net-close socket) true)))

(define (confirm-request conf)
    (net-receive socket recvbuff 256 "\r\n")
    ; Empty out pipe. According to SMTP spec, last line has valid code.
    ; added for 1.8 for newLISP 9.2.0
    (while (< 0 (net-peek socket))
        (net-receive socket recvbuff 256 "\r\n") )
    (starts-with recvbuff conf))

(define (net-send-get-result str conf)
   (set 'send-str (string str "\r\n"))
   (net-send socket send-str)
   (if conf (confirm-request conf) true))

; DANGER! We *must* use 'append' here instead of 'string' as the two treat "\000" differently!
(define (mail-authorize user-name password)
   (net-send-get-result
       (string "AUTH PLAIN "
               (base64-enc (append "\000" user-name "\000" password))) "235"))

; ;old functions, we have our own.
; (define (mail-send-header)
;     (net-send-get-result (string "TO: " mail-to))
;     (net-send-get-result (string "FROM: " mail-from))
;     (net-send-get-result (string "SUBJECT: " mail-subject))
;    ;(net-send-get-result headers)
;     (net-send-get-result (string "X-Mailer: newLISP v." (nth -2 (sys-info)))))
;
; (define (mail-send-body )
;     (net-send-get-result "")
;     (dolist (lne (parse mail-body "\r\n"))
;         (if (starts-with lne ".")
;             (net-sent-get-result (string "." lne))
;             (net-send-get-result lne)))
;     (net-send-get-result "."))

;; @syntax (SMTP:get-error-text)
;; <p>Call this to get the reason 'send-mail' returned 'nil'.</p>
(define (get-error-text)
    recvbuff)

; ---------------------------------------------------------------
; !Attachments - Public API
; ---------------------------------------------------------------

;; @syntax (SMTP:clear-attachments)
(define (clear-attachments)
   (setf attachments '()) )

;; @syntax (SMTP:attach-document <str-content> <str-filename> [<str-disposition> [<str-mime-type> [<str-encoding>]]])
;; @param <str-content> The attachment data.
;; @param <str-filename> How you'd like your attachment to appear named in the email.
;; @param <str-disposition> "attachment" or "inline". default is "attachment".
;; @param <str-mime-type> default is "application/octet-stream".
;; @param <str-encoding> default is "base64". If 'encoding' is "base64" it will be automatically transformed using 'encode64-widthsafe'
(define (attach-document content filename (disposition "attachment") (mime-type "application/octet-stream") (encoding "base64"))
   (push (list content filename disposition mime-type encoding) attachments -1) )

; ---------------------------------------------------------------
; !UTF-8 encoding support for non-ASCII characters
; ---------------------------------------------------------------

;; @syntax (SMTP:encode64-widthsafe <buff-data>)
;; <p>Useful for attaching binary data such as images. Converts the data into base64
;; and chops it up so that the lines are not longer than 76 characters long, making
;; it safe to include in the body of emails.</p>
;; <p>If the attachment's encoding to "base64" (which it is by default), this function
;; will automatically applied to the <str-content> of the email.</p>
;; Fixed "bare lf's in body" error to make servers using RFC822 Spam filtering happy V3.x
;
(define (encode64-widthsafe data)
   (join (explode (base64-enc data) 76) "\r\n")  )

;; @syntax (SMTP:encode64-line <str-line>)
;; <p>Creates a base64 UTF-8 compatible string, suitable for including foreign characters
;; in the subjects of emails. This is used by 'send-mail' automatically on the filename
;; of any attachments, as well as the subject of the email.</p>
(define (encode64-line str)
   (string "=?UTF-8?B?" (base64-enc str) "?=") )

; ---------------------------------------------------------------
; !Attachments - Private API
; ---------------------------------------------------------------

(setf boundary (string "newLISP-" (nth -2 (sys-info)) "--65z64F4n654"))
(setf headers (string "MIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=" boundary))

(setf mail-body-wrapper (string
{--} boundary {
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: base64

%s
   
--} boundary {%s}))

; filename madness. We actually do not need the *=utf-8 weirdness
; if we're using the encode64-line func instead of utf8-urlencode

(setf attachment-wrapper (string
;{Content-Disposition: %s; filename*=utf-8''%s
{Content-Disposition: %s; filename="%s"
Content-Type: %s; name="%s"
Content-Transfer-Encoding: %s

%s

--} boundary {%s}))

(setf attachments '()) ; the list of attachments is placed here

(define (prepared-body)
   (format mail-body-wrapper (encode64-widthsafe mail-body)
      ; indicate this is the last boundary if no attachments
      (if (zero? (length attachments)) "--" "")) )
;
; This crude gettimezone hack only works for USA on Win32
; someone else can fix it for the rest of the world
; Removed (encode64-line on subject to reduce SpamAssasin value
;
(define (gettimezone ,tmp)
   (set 'tmp (now)
        'tmp (/ (+ (tmp 9) (tmp 10)) 60))
   (string "-0" tmp "00") )
;
; Removed (encode64-line on Subject to to improve sanity and get by some spam filters:)
; Added Date using a gettimezone hack to avoid DATE_MISSING spam test
;
(define (mail-send-header)
    (net-send-get-result (string "TO: " mail-to) )
    (net-send-get-result (string "FROM: " mail-from) )
    (net-send-get-result (string "DATE: " (date (date-value) 0 "%a, %d %b %Y %X ") (gettimezone)) )
#;    (net-send-get-result (string "SUBJECT: " (encode64-line mail-subject)))
    (net-send-get-result (string "SUBJECT: " mail-subject))
    (net-send-get-result headers)
    (net-send-get-result (string "X-Mailer: newLISP v." (nth -2 (sys-info)) "\r\n")) )

(define (mail-send-body)
   (net-send-get-result "")
   (net-send-get-result (prepared-body))
   (send-attachments)
   (net-send-get-result ".") )

(define (send-attachments , encoding filename)
   (dolist (attachment attachments)
      (set 'encoding (attachment 4) 'filename (attachment 1))
      (net-send-get-result (format attachment-wrapper
         (attachment 2)
         (encode64-line filename)
         (attachment 3)
         (encode64-line filename)
         encoding
         (if (= encoding "base64")
            (encode64-widthsafe (attachment 0))
            (attachment 0) )
         ; indicate this is the last boundary if no more
         (if (= (+ 1 $idx) (length attachments)) "--" "")
      ))))
(context MAIN)

