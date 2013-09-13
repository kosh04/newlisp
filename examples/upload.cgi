#!/usr/bin/env newlisp
#
# upload.cgi - process uploads from POST multipart/form-data
#
# version 1.7
#
# works on Apache server 1.2 and after and newLISP server 10.3.0 and after
# works on newLISP CGI with 10.1 and after

; make compatible with older versions of newLISP
(when (< (sys-info -2) 10110)
    (constant (global 'write) write-buffer))

(define (process-upload,
		; locals
		len type infile outfile boundary
		disposition filename start end size
		bytesread buffer)

	(print "Content-Type: text/html\n\n")

	; check for valid request header
	(set 'len (int (env "CONTENT_LENGTH") 0))
	(set 'type (env "CONTENT_TYPE"))
	(if (not (find ".*multipart/form-data;.*boundary=.*" type 1))
	      (throw-error "wrong upload format, multipart/form-data content-type not found"))

	; read data into intermediate file
	(set 'infile (open "upload-file" "write"))
	(while (read-buffer (device) buffer 1024)
	    (write infile buffer))
	(close infile)
	; get filename and boundaries
	(set 'infile (open "upload-file" "read"))
	(set 'boundary (read-line infile)) ; get boundary string
	(set 'disposition (read-line infile))

	(if (not (find ".*filename=\"(.*)\".*" disposition 1))
		(throw-error "wrong upload format, did not find filename"))
	(if (= "" (set 'filename $1))
		(throw-error "need file name")
		(set 'filename (last (parse filename "/|\\\\" 0))))

	; read from intermedeate file into permanent file
	(while (!= "" (read-line infile)))
	(set 'start (seek infile))
	(set 'end (search infile boundary))
	(if (not end)
		(throw-error "wrong upload format, did not find boundary string")
		(set 'size (- end start 2)))

	(set 'outfile (open filename "write"))
	(seek infile start)
	(while (> size 0)
	    (set 'bytesread (read-buffer infile buffer (min 1024 size)))
	    (write outfile buffer bytesread)
	    (dec size bytesread))
	(close infile)
	(close outfile)
	(delete-file "upload-file"))

(if (not (catch (process-upload) 'result))
	(print "<h2>" (first (parse result "\\r\\n|\\n" 0)) "</h2>")
	(print {<h2>file uploaded, click <a href="upload.html">continue</a></h2>}))

(exit)
