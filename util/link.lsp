;; link.lsp - v.1.1
;; link.lsp - v.1.2 fix for '.exe' problem on CYGWIN
;; link.lsp - v.1.3 no code change, added note for win32 dll
;; link.lsp - v.1.4 no code change, reformatted comments 
;; link.lsp - v.1.5 no code change, changed dllEvalStr to newlispEvalStr 
;;
;; links newLISP executable and a .lsp source file to a new
;; executable containing both. This way newLISp and a source
;; file can be packaged together for easier distribution and install.
;;
;; 
;; NOTES FOR LINKING AND IMPORTING WIN32 DLLs
;; Since newLISP v.7.3.16 CYGWIN is not supported anymore for
;; linking source to the executable, use a native Win32 instead
;; which also can be used in an CYGWIN environment
;;
;; When importing a linked win32 dll from win32 programs other
;; then newlisp.exe, than the following statment should be executed
;; directly after importing functions and before any 'newlispEvalStr'
;; statment: (dllName <name-of-dll>) where <name-of-dll> is the file
;; name of the linked win32 newlisp dll. This will enable the autoload
;; of linked newlisp source in the dll. When importing to newlisp.exe
;; this is not required.
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
;; How to use on CYGWIN WIn32:
;; ===========================
;; example: to link the source mySource.lsp to myProg.exe
;;
;; (link "newlisp.exe" "myProg.exe" "mySource.lsp")
;;
;; then:
;;
;; c:\> myProg
;;
;; 
;; On Linux/freeBSD implementations:
;; =================================
;; (link "newlisp" "myProg" "mySource.lsp")
;;
;; and to execute myProg assuming myProg is in the current directory
;;
;; /home/joe > ./myProg
;;


(define (link orgExe newExeName lispSourceName)
    (set 'size (first (file-info orgExe)))

    ;; copy newLISP.exe to the newExeName
    (copy-file orgExe newExeName)

    ;; get the binary representation of size
    (set 'buff (pack "ld" size))

    ;; open the new exe for update
    (set 'handle (open newExeName "u"))
    (search handle "@@@@@@@@")

    ;; this field gets read by the newLISP startup
    (write-buffer handle 'buff 4)
    (set 'buff (read-file lispSourceName))
    (set 'keylen (pack "ld" (length buff)))
    (write-buffer handle 'keylen 4)

    ;; append lisp source encrypted at the end
    (seek handle size)
    (set 'buff (encrypt buff (string (length buff))))
    (write-buffer handle 'buff (length buff))
    (close handle))

;; eof

;; test
;;(link "newlisp" "qaprog" "qa")

