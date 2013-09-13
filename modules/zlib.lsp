;; @module zlisp.lsp
;; @description Functions for compression/decompression with zlib
;; @version 1.1 - comments redone for automatic documentation
;; @version 1.2 - new library detection routine
;; @version 1.3 - added lib for opnBSD and tested for 64-bit newLISP
;; @version 1.4 - doc changes
;; @version 1.5 - replaced <tt>write-buffer</tt> with <tt>write</tt>
;; @version 1.6 - removed broken <tt>squeeze</tt> and <tt>unsqueeze</tt> functions.
;; @author L.M 2006-2013
;; <h3>Functions for compression/decompression with zlib</h3> 
;; For this module a platform sepcific library
;; from @link http://www.zlib.net/ www.zib.net is needed.
;;
;; The module offers two compression/decompression support as
;; GZ compatible file compression and decompression.
;; 
;; Before using the module it must be loaded:
;; <pre>
;; (load "/usr/share/newlisp/modules/zlib.lsp")
;; ; or shorter
;; (module "zlib.lsp")
;; </pre>

(when (< (sys-info -2) 10110)
	(constant (global 'write) write-buffer))

(context 'zlib)

(set 'files '(
	"/usr/lib/libz.so" ; Linux, BSD, Solaris
	"/usr/lib/libz.so.4.1" ; OpenBSD 4.6
	"/usr/lib/libz.dylib" ; Mac OSX / Darwin
	"libz1.dll" ; Win32
))

(set 'library (files (or
		       (find true (map file? files))
		       (throw-error "cannot find zlib compression library"))))

(import library "compress")
(import library "uncompress")
(import library "gzopen")
(import library "gzread")
(import library "gzclose")
(import library "gzwrite")

;; @syntax (zlib:gz-read-file <str-file-name>)
;; @return A string buffer with the original contents.
;;
;; Uncompresses the GZ compressed file in <str-file-name>.
;; @example
;; (set 'buff (zlib:gz-read-file "myfile.gz"))

(define (gz-read-file file-name)
	(let ( (fno (gzopen file-name "rb"))
	       (buff (dup "\000" 0x1000))
	       (result ""))
		(if (!= fno 0)
			(begin
				(while (> (set 'bytes (gzread fno buff 0x1000)) 0)
					(write result buff bytes))
				(gzclose fno)
				result))))

;; @syntax (zlib:gz-write-file <str-file-name> <str-buffer>)
;; @return The number of bytes in <str-buffer>.
;;
;; Does a GZ compatible comrpression of a buffer in <str-buffer> and
;; writes it to the file in <str-file-name>.
;; @example
;; (zlib:gz-write-file "myfile.gz" buff) 

(define (gz-write-file file-name buff)
    (let ( (fno (gzopen file-name "wb"))
		   (result nil))
    	(if (!= fno 0)	
			(begin
				(set 'result (gzwrite fno buff (length buff)))
				(gzclose fno)
				result))))
	
(context MAIN) 

; eof

