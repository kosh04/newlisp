;; @module smtpx.lsp
;; @description Send mail using SMTP protocol. Alternate smtp module handling attachments.
;; @version 2.0 - March 2008, Cormullion added AUTH PLAIN authentication
;; @version 2.1 - changes for 10.0
;; @version 2.2 - (2009-07-16) Alessandro Manotti, Added multi-attachments support. Small bug-fixes.
;; @version 2.2.1 - (2009-07-17) Alessandro Manotti and SMTP:list-attachments, online help
;; @version 2.2.2 - removed SMTP:info? online help support
;; @author Lutz Mueller 2001, Cormullion 2008, Alessandro Manotti 2009
;;
;; <h2>Routines for sending mail</h2>
;; This module implements routines to communicate with a SMTP mail server
;; for sending email. This is an enhanced implementaton of the older 'smtp.lsp' 
;; module which did not allow attachments. After more testing is done, this module
;; will replace the older 'smtp.lsp'. Currently this module has only been tested
;; with MS Exchange server.
;;
;; To use this module include the following 'load' statement
;; at the beginning of the program file:
;; <pre>
;; (load "/usr/share/newlisp/modules/smtpx.lsp")
;; ; or as an alternative on a standard newLISP install
;; (module "smtpx.lsp")
;; </pre>
;; To see debugging information:
;; <pre>(set 'debug-flag true)</pre>
;; In order to use attachments, use the following flow (this is an example):
;; <pre>
;; (SMTP:clear-attachments)
;; (SMTP:add-attachment "/home/ale/temp/" "lisp.pdf" "This is a PDF file" "application/pdf")
;; (SMTP:add-attachment "/home/ale/temp/" "myimage.jpg" "My house!" "image/jpg")
;; (SMTP:send-mail "from_user@test.com" "to_user@test.com" 
;;                 "This is email subject" "This is the message-body!" 
;;                 "My server IP" "userid" "passerword")
;; </pre>
;; As you can see, you need to clear attachments list (that list is NOT automatically deleted, so you can
;; send the same attachments at different email addresses, by using (SMTP:send-mail) multiple times).
;; Then you can add your attachments, using the function (SMTP:add-attachment) with the following syntax:
;; <pre>
;; (SMTP:add-attachment file-path file-name attachment-name mime-type)
;; </pre>
;; Then use '(SMTP:send-mail)' as usual.
;; NOTE: you can add as many attachments as you want.

(context 'SMTP)

(set 'debug-flag nil)

;; @syntax (SMTP:send-mail <str> <str> <str> <str> <str> [<str> str-pass>]])
;; @param <str> The email address of the sender.
;; @param <str> The email address of the recipient.
;; @param <str> The subject line of the email.
;; @param <str> The message part of the email.
;; @param <str> The address of the SMTP server.
;; @param <str> Optional user name for authentication.
;; @param <str> Optional password for authentication.
;; @return On success 'true', on failure 'nil'.
;; In case the function fails returning 'nil', the function
;; 'SMTP:get-error-text' can be used to receive the error text.
;;
;; @example
;; (SMTP:send-mail "jdoe@asite.com" "somebody@isp.com" "Greetings"
;;   "How are you today? - john doe -" "smtp.asite.com" "jdoe" "secret")

;; This logs in to the server, tries to authenticate using the username 'jdoe' and password 'secret' (if supplied),
;; and sends an email with the format:
;; <pre>
;;  From:    jdoe@asite.com
;;  To:      somebody@isp.com
;;  Subject: Greetings
;;  Message: How are you today? - John Doe -
;; </pre>
;;
;; ATTACHMENTS MANAGEMENT:
;; @syntax (SMTP:clear-attachments)
;; Clear the attachments list.
;;
;; @syntax (SMTP:list-attachments)
;; Show the list of the files currently attached; they will be sent using (SMTP:send-mail).
;;
;; @syntax (SMTP:add-attachment <str> <str> <str> <str>)
;; @param <str> Path of the file, terminating with "/".
;; @param <str> Name of the file to be attached.
;; @param <str> Attachment name (usually, it is the same name of the file-name).
;; @param <mime> Mime-type of the file.
;;

(context 'SMTP)

; Files (filenames) that will be attached to the emails.
; This variable contains the following structure:
;   ( (attachment_1_info) (attachment_2_info) ... )
;   
;   Where:
;       (attachment_1_info)
;           It is composed in this way:
;           (file-path file-name attachment-name mime-type)
;
;           file-path = [String] Path of the file, terminating with "/".
;                       Example (Linux): /home/ale/temp/
;                       Example (Win):   c:/ale/temp/
;
;           file-name = [String] Name of the file to be attached.
;
;           attachment-name = [String] Attachment name (usually, it is the same name of the file-name).
;
;           mime-type = [String] Mime-type of the file.
;                       Example:
;                       JPG image   -> "image/jpg"
;                       PNG image   -> "image/png"
;                       html file   -> "text/plain"
;
;                       More info about mime-types can be found at:
;                       http://www.iana.org/assignments/media-types/
;                       http://www.w3schools.com/media/media_mimeref.asp
;                       http://en.wikipedia.org/wiki/MIME
;
(setq attachmentsList '())

;; Key used to separate multi-parts.
(setq boundaryKey (string "==NextPart_H725_KHH93DBO77KK." (time-of-day)))

(if debug-flag
  (println "Auto-generated boundaryKey: " boundaryKey))


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
      (or (net-close socket) true))
);send-mail


(define (confirm-request conf)
    (net-receive socket 'recvbuff 256 "\r\n")
   
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
      (base64-enc (append "\000" user-name "\000" password))) "235")
)


(define (mail-send-header)
  (net-send-get-result (append "TO: " mail-to))
  (net-send-get-result (append "FROM: " mail-from))
  (net-send-get-result (append "SUBJECT: " mail-subject))     
)


(define (mail-send-body)
    (net-send-get-result (string {Content-Type: multipart/mixed; boundary="} boundaryKey {"}))
    (net-send-get-result "Mime-Version: 1.0")
    (net-send-get-result (append "X-Mailer: newLISP v." (string (nth -2 (sys-info)))))
    (net-send-get-result "")       

    (net-send-get-result "")       
    (net-send-get-result (append "--" boundaryKey))
    (net-send-get-result "Content-Type: text/plain" )
    (net-send-get-result "Content-Transfer-Encoding: 7bit" )
    (net-send-get-result "")           
    (dolist (lne (parse mail-body "\r\n"))
        (if (starts-with lne ".")
            (net-send-get-result (append "." lne))
            (net-send-get-result lne)))

    ;; If there are some attachments, put them in the email!
   
    (dolist (i attachmentsList)
        (if debug-flag (begin
          (println "Preparing attachment...")
          (println "   argFilePath       : " (i 0))
          (println "   argFilename       : " (i 1))
          (println "   argAttachmentName : " (i 2))
          (println "   argMimeType       : " (i 3))
        ))

        (prepare-attachment-for-sending (i 0) (i 1) (i 2) (i 3) )
       
        (if debug-flag (println "   Done! -------------------------------"))

    );dolist
   
    (net-send-get-result (append "--" boundaryKey "--"))
    (net-send-get-result ".")
);mail-send-body

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to manage attachments.
;;
;; NOTE: after you send the email with attachments, such list is not cleared,
;; so you will be able to send another email using the same list!
;; Instead, to clear it, simply call (clear-attachments)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show current attachments list.
;;
(define (list-attachments)
  (dolist (i attachmentsList)
    (println "File Path       : " (i 0))
    (println "File Name       : " (i 1))
    (println "Attachment Name : " (i 2))
    (println "Mime-Type       : " (i 3) "\n")
  )
 
  (silent (print "> "))
);list-attachments

;; Clear attachments list.
;;
(define (clear-attachments)
  (setq attachmentsList '())
  (if debug-flag
    (println "Attachments cleared!")
    (println "Current attachment list: " attachmentsList)
  )
);clear-attachments

;; Add another attachment to be sent via email.
;;
(define (add-attachment argFilePath argFilename argAttachmentName argMimeType)
    (push (list argFilePath argFilename argAttachmentName argMimeType) attachmentsList -1)
);add-attachment

;; Link a single attachment to the email (prepare attachment to be sent).
;;
(define (prepare-attachment-for-sending argFilePath argFilename argAttachmentName argMimeType)
    (net-send-get-result "")
    (net-send-get-result (append "--" boundaryKey))
    (net-send-get-result (append {Content-Disposition: attachment; filename="} argFilename {"} ) )
    (net-send-get-result (append {Content-Type: } argMimeType {; name="} argAttachmentName {"} ) )
    (net-send-get-result "Content-Transfer-Encoding: base64" )
    (net-send-get-result "")
    (net-send socket (base64-enc (read-file (append argFilePath argFilename) )))
    (net-send-get-result "")
    (net-send-get-result "")
   
);prepare-attachment-for-sending

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

