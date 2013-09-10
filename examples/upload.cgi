#!/usr/bin/newlisp
#
# Upload script v. 1.0
# v 1.1 - changed 'integer' to 'int'
# v 1.2 - eliminated quote in write-buffer syntax
# v 1.3 - change in read-buffer for version 10.0
# serves POST request from upload.html
# 
#

(define (process-upload,
		; locals
		len type infile outfile boundary
		disposition filename start end size
		bytesread buffer)

	(print "Content-type: text/html\n\n")

	; check for valid request header
	(set 'len (int (env "CONTENT_LENGTH") 0))
	(set 'type (env "CONTENT_TYPE"))
	(if (not (find ".*multipart/form-data;.*boundary=.*" type 1))
	      (throw-error "wrong upload format"))

	; read data into intermediate file
	(set 'infile (open "upload-file" "write"))
	(while (!= (read-buffer (device) buffer 1024) 0)
	    (write-buffer infile buffer))
	(close infile)

	; get filename and boundaries
	(set 'infile (open "upload-file" "read"))
	(set 'boundary (read-line infile)) ; get boundary string
	(set 'disposition (read-line infile))

	(if (not (find ".*filename=\"(.*)\".*" disposition 1))
		(throw-error "wrong upload format"))
	(if (= "" (set 'filename $1))
		(throw-error "need file name")
		(set 'filename (last (parse filename "/|\\\\" 0))))

	; read from intermedeate file into permanent file
	(while (!= "" (read-line infile)))
	(set 'start (seek infile))
	(set 'end (search infile boundary))
	(set 'size (- end start 2))

	(set 'outfile (open filename "write"))
	(seek infile start)
	(while (> size 0)
	    (set 'bytesread (read-buffer infile buffer (min 1024 size)))
	    (write-buffer outfile buffer bytesread)
	    (dec size bytesread))
	(close infile)
	(close outfile)
	(delete-file "upload-file"))

(if (not (catch (process-upload) 'result))
	(print "<h2>" (first (parse result "\\r\\n|\\n" 0)) "</h2>")
	(print {<h2>file uploaded, click <a href="upload.html">continue</a></h2>}))

(exit)
