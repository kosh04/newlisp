;; @module ftp.lsp
;; @description FTP file transfer routines
;; @version 1.4 - comments redone for automatic documentation
;; @version 1.5 - change in read-buffer for v.10.0
;; @version 1.6 - change in net-receive for v.10.0
;; @version 1.7 - doc changes
;; @version 1.8 - <tt>write</tt>, <tt>read</tt> and compatibility with pre v10.1.11
;; @author Eddie Rucker, Lutz Mueller, 2003-2010
;; <h2>FTP file transfer routines</h2>
;; To use the module put a 'load' statement at beginning of your
;; program file:
;; <pre>
;; (load "/usr/local/share/newlisp/modules/ftp.lsp")
;; ; or shorter
;; (module "ftp.lsp")
;; </pre>
;; In case of failure the functions return 'nil' and further detail
;; may be found in the variable 'FTP:result'.
;;
;; To set debug mode, which shows all dialog with the server, set
;; 'FTP:debug-flag':
;; 
;; <tt>(set 'FTP:debug-flag true)</tt>

; compatibility with versions older than 10.1.11
(when (< (sys-info -2) 10111)
	(constant (global 'write) write-buffer)
	(constant (global 'read) read-buffer))

(context 'FTP)

; debugging mode
(set 'debug-mode nil)

; mode of transfer
(define GET 1)
(define PUT 2)

;; @syntax (FTP:get <str-user-id> <str-password> <str-host> <str-dir> <str-file-name>)
;; @param <str-user-id> The user ID for logon.
;; @param <str-password> The password for the user ID.
;; @param <str-host> The remote host name or IP as a string.
;; @param <str-dir> The subdirectory on the host.
;; @param <str-file-name> The name of the file to transfer.
;; @return 'true' on success, 'nil' on failure.
;; @example
;; (FTP:get "somebody" "secret" "host.com" "subdir" "aFile.tgz")  ;; download
;; (FTP:get "somebody" "secret" "192.168.1.120" "" "myfile.txt")  ;; download

;; When leaving the string for the sub directory empty, the current directory "."
;; is assumed on the host.

(define (get user-id password host subdir file-name)
    (transfer user-id password host subdir file-name GET))

;; @syntax (FTP:put <str-user-id> <str-password> <str-host> <str-dir> <str-file-name>)
;; @param <str-user-id> The user ID for logon.
;; @param <str-password> The password for the user ID.
;; @param <str-host> The remote host name or IP as a string.
;; @param <str-dir> The sub directory on the host.
;; @param <str-file-name> The name of the file to transfer.
;; @return 'true' on success, 'nil' on failure.
;; @example
;; (FTP:put "somebody" "secret" "host.com" "subdir" "file")  ;; upload

(define (put user-id password host subdir file-name)
    (transfer user-id password host subdir file-name PUT))

(define (transfer user-id password host subdir file-name mode)
  (if (= subdir "") (set 'subdir "."))
  (and
    (connect-to host 21)
    (send-get-result (append "USER " user-id "\r\n") "3")
    (send-get-result (append "PASS " password "\r\n") "2")
    (send-get-result (append "CWD " subdir "\r\n") "2")
    (send-get-result "TYPE I\r\n" "2")
    (set 'buff (send-get-result "PASV\r\n" "2"))
    (regex {(\d+),(\d+),(\d+),(\d+),(\d+),(\d+)} buff)
    (set 'port (+ (* 256 (int $5)) (int $6)))
    (set 'ip (string $1 "." $2 "." $3 "." $4))
    (set 'socket2 (net-connect ip port))

    (if (= mode PUT)
        (and
            (check-file file-name)
            (net-send socket (append "STOR " file-name "\r\n"))
            (send-get-result "STAT\r\n" "1")
            (set 'fle (open file-name "r"))
            (while (> (read fle buffer 512) 0)
                (if debug-mode (print "."))
                (net-send socket2 buffer 512))
            (close fle)) true)

    (if (= mode GET)
        (and
            (net-send socket (append "RETR " file-name "\r\n"))
            (send-get-result "STAT\r\n" "1")
            (set 'fle (open file-name "w"))
            (while (net-receive socket2 buffer 512)
                (if debug-mode (print "."))
                (write fle buffer))
            (close fle)) true)

    (or (net-close socket2) true)
    (net-send socket "QUIT\r\n")
    (or (net-close socket) true)))

(define (send-get-result str code)
    (net-send socket str)
    (if debug-mode (println "sent:" str))
    (net-receive socket result 256 "\r\n")
    (if debug-mode (println result))
    (if (starts-with result code) result))

(define (connect-to host port)
    (set 'FTP:result nil)
    (set 'socket (net-connect host port))
    (if socket
        (net-receive socket result 256 "\r\n")
        (begin
            (set 'result "could not connect")
            nil)))    
    
(define (check-file file-name)
    (if (file? file-name)
        true
        (begin
            (set 'result (append file-name " does not exist"))
            nil)))

(context 'MAIN)

; test
;
;(set 'FTP:debug-mode true)
;
;(FTP:put "userid" "password" "site.com" "tmp" "testfile")
;
;(FTP:get "userid" "password" "site.com" "tmp" "testfile")
;
;(exit)



; eof


