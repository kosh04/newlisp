;; link.lsp - v.2.0 for newLISP v10.4.7 and later
;;
;; Linking a newLISP exeutable with source code is now a built-in
;; commandline option: newlisp -x source.lsp source.exe. So this
;; file is not really required anymore, but has been adapted to
;; show the different way linkage is handled now.
;;
;; The following just shows what is happening internally.
;; When using the -x option the running executable newLISP
;; is used as the original file to link too. In this
;; script a different executable can be chosen.

(define (link orgExe newExeName lispSourceName)
	(println "original newlisp executable:" orgExe)
	(println "new executable:" newExeName)
	(println "source:" lispSourceName)

    (set 'size (first (file-info orgExe)))

    ;; copy newLISP.exe to the newExeName
    (copy-file orgExe newExeName)

    ;; get the binary representation of size
    (set 'buff (pack "ld" size))

    ;; open the new exe for update
    (set 'handle (open newExeName "u"))
    (search handle "&&&&@@@@")

    ;; this field gets read by the newLISP startup
    (write-buffer handle buff 4)
    (set 'buff (read-file lispSourceName))
    (set 'keylen (pack "ld" (length buff)))
    (write-buffer handle keylen 4)

    ;; append lisp source at the end
    (seek handle size)
    (write-buffer handle buff (length buff))
    (close handle)
)

;; eof


