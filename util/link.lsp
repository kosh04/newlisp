;; link.lsp - v.1.8
;;
;; links newLISP executable and a .lsp source file to a new
;; executable containing both. This way newLISp and a source
;; file can be packaged together for easier distribution and install.
;; 
;; When importing a linked win32 dll from win32 programs other
;; then newlisp.exe, than the following statment should be executed
;; directly after importing functions and before any 'newlispEvalStr'
;; statment: (dllName <name-of-dll>) where <name-of-dll> is the file
;; name of the linked win32 newlisp dll. This will enable the autoload
;; of linked newlisp source in the dll. When importing to newlisp.exe
;; this is not required.
;;
;; The utility should only be used on MS Windows. On Mac OSX and
;; other UNIX platforms, the resulting executable will only work
;; when present in and executed from the current directory. Only
;; on MS Windows, the resulting executable can be put anywhere
;; in the execution path of the OS.
;;
;; How it works:
;; =============
;; copies the original newLISP.exe to newExeName
;; then searches for '@@@@@@@@', which is the place holder
;; for the exe offset to append lisp source and for a simple
;; encryption key derived from the source size
;; the file in lispSourceName gets encrypted and appended
;; to the a copy of newLISP.exe with a new name.
;;
;; the new executable file will not load init.lsp but
;; the appended file instead
;;
;; How to use on Win32:
;; ====================
;; example: to link the source mySource.lsp to myProg.exe
;;
;; (link "newlisp.exe" "myProg.exe" "mySource.lsp")
;;
;; then:
;;
;; c:\> myProg
;;


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
    (search handle "@@@@@@@@")

    ;; this field gets read by the newLISP startup
    (write-buffer handle buff 4)
    (set 'buff (read-file lispSourceName))
    (set 'keylen (pack "ld" (length buff)))
    (write-buffer handle keylen 4)

    ;; append lisp source encrypted at the end
    (seek handle size)
    (set 'buff (encrypt buff (string (length buff))))
    (write-buffer handle buff (length buff))
    (close handle))

;; eof

;; test
;;(link "newlisp" "qaprog" "qa")

