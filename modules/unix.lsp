;; @module unix.lsp
;; @description Itreface to various UNIX libc functions
;; @version 0.1 alpha - mostly untested
;; @author L.M. 2006-Sep-12, lutz@nuevatec.com
;;
;; <h2>UNIX libc function support</h2>
;; To use this module include the following line at the beginning of
;; the program file:
;; <pre>
;; (load "/usr/share/newlisp/modules/unix.lsp")
;; ; or shorter
;; (module "unix.lsp")
;; </pre>
;; All functions are named like the UNIX functions they implelement but with
;; the unix: namespace prefix. All functions work similar to the UNIX libc functions
;; they implement but return 'nil' instead of '-1' for failure and 'true' instead
;; of 0 for success. Note that in the implementaion most parameter types are casted
;; even though redundant in most situation. The addional cast avoids segfaulting
;; of the imported functions when supplied with wrong parameter types, i.e. for strings.
;; Some times the 'int' cast is used to supply default values of an argument, in case
;; the argument is not given.
;;
;; The arguments are given in, is the order used by the equivalent libc functions, not
;; the order as used by shell  commandline utilities with the same name.
;;
;; Some of the wordings from functions descriptions are taken from BSD man pages.
;;
;; This module contains wrappers for the following libc functions:
;; 'getpid', 'getppid', 'getuid', 'geteuid', 'getgid', 'getegid', 
;; 'setuid', 'seteuid', 'setgid', 'setegid',
;; 'kill', 'chmod', 'ioctl', 'mkfifo', 'mltemp', 'syslog'
;; 

(context 'unix)

(if
    ;; LINUX and BSDs
    (< (& 0xF (last (sys-info))) 3) (set 'library "/usr/lib/libc.so")
    ;; Mac OSX / Darwin
    (= (& 0xF (last (sys-info))) 3) (set 'library "/usr/lib/libc.dylib")
    ;; Solaris
    (= (& 0xF (last (sys-info))) 4) (set 'library "/usr/lib/libc.so")
    true (throw-error "Cannot load library")
)

;; @syntax (unix:getpid)
;; @return Returns the current process ID

(import library "getpid")

;; @syntax (unix:getppid)
;; @return Returns the parents process ID.

(import library "getppid")

;; @syntax (unix:getuid)
;; @return Returns the real and effective user ID

(import library "getuid")`

;; @syntax (unix:geteuid)
;; @return Returns the effective user ID

(import library "geteuid")

;; @syntax (unix:getgid)
;; @return Returns the real group ID.

(import library "getgid")

;; @syntax (unix:getegid)
;; @return Returns the effective group ID

(import library "getegid")

;; @syntax (unix:setuid <num-id>)
;; @param <num-id> The number of the real and effective user IDs to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Set the real and effective user IDs.
;; If no argument or the argument given is not a number then sets
;; current real and effective user IDs.

(set '_setuid (import library "setuid"))

(define (setuid id)
	(zero? (_setuid (int id (getuid))))
)

;; @syntax (unix:seteuid <num-id>)
;; @param <num-id> The number of the effective user ID to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Sets the effective user ID.
;; If no argument or the argument given is not a number then sets
;; current effective user ID.
;;
(set '_seteuid (import library "seteuid"))

(define (seteuid id)
	(zero? (_seteuid (int id (geteuid))))
)


;; @syntax (unix:setgid <num-id>)
;; @param <num-id> The number of the real and effective group ID to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Sets the real and effective group IDs.
;; If no argument or the argument given is not a number then sets
;; current real and effective group IDs.

(set '_setgid (import library "setgid"))

(define (setgid id)
	(zero? (_setgid (int id (getgid))))
)


;; @syntax (unix:setegid <num-id>)
;; @param <num-id> the number of the effective group ID to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Sets the effective group ID.
;; If no argument or the argument given is not a number then sets
;; current effective group ID.

(set '_setegid (import library "setegid"))

(define (setegid id)
	(zero? (_setegid (int id (getegid))))
)

;; @syntax (unix:kill <num-pid> <num-sig>)
;; @param <num-pid> The process ID of the process to send a signal to.
;; @param <num-sig> The signal to send to the process in <num-pid>
;; @return Returns 'true' or 'nil' depending on success
;;
;; Send a signal to a process.
;; If no argument is given for <num-sig> or <num-sig> is zero, then
;; the validiy of the <num-pid> is checked and 'nil' is returned for
;; an invalid <num-pid>.
;; Note that kill works has parameters backwards compared to the
;; kill used in on the shell command line. '(unix:kill ...)' works
;; like the libc 'kill()' counterpart.

(set '_kill (import library "kill"))

(define (kill pid sig)
	(zero? (_kill (int pid 0) (int sig 0)))
)

;; @example
;; (unix:kill 2652 9)

;; Sends the 'SIGKILL' signal 9 to process ID '2652'

;; @syntax (unix:chmod <str-path> <num-mode>)
;; @param <str-path> The path name of the file or directory
;; @param <num-option> The permission pattern as a number
;; @return Returns 'true' or 'nil' depending on success
;;
;; Sets the permission pattern for a UNIX filesystem node. The
;; pattern is best given as an octal number, i.e.: '0755' for 
;; a -rwxr-xr-x permission pattern, '0644' for '-rw-r--r--' etc.
;; If no <int-option> is given unix:chmod assumes '0644'.

(set '_chmod (import library "chmod"))

(define (chmod path option)
	(zero? (_chmod (string path) (int option 0644)))
)

;; @example
;; (unix:chmod "myfile.txt" 0644)

;; Gives '-rw-r--r--' permission to the file 'myfile.txt'

;; @syntax (unix:ioctl <num-id> <num-request> <str-argp>)
;; @param <num-id> A number for file descriptor.
;; @param <num-request> A number for a request.
;; @param <str-argp> An argument string.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Manipulates the underlying device parameters of special
;; files.
;;
(set '_ioctl (import library "ioctl"))

(define (ioctl id request argp)
	(zero? (_ioctl (int id 0) (int request 0) (string argp)))
)

;; @syntax (unix:mkfifo <str-path> <num-mode>)
;; @param <str-path> The path  of the new fifo filename.
;; @param <num-mode> The persmissions mode number.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Creates a new fifo file with name path.  The access permissions
;; are specified by mode and restricted by the 'umask(2)' of the calling
;; process.

(set '_mkfifo (import library "mkfifo"))

(define (mkfifo path mode)
	(zero? (_mkfifo (string path) (int mode)))
)

;; @example
;; (unix:mkfifo "myfifo" 0755)

;; This will create a fifo node with 'prwxr-xr-x' permissions


;; @syntax (unix:mktemp <str-template>)
;; @param <str-template> A file template with Xs appended, i.e. '/tmp/temp.XXXX'.
;; @return Returns 'true' or 'nil' depending on success
;;
;; The function takes the given file name template and overwrites a
;; portion of it to create a file name.  This file name is guaranteed not to
;; exist at the time of function invocation and is suitable for use by the
;; application.  The template may be any file name with some number of 'X's
;; appended to it, for example '/tmp/temp.XXXXXX'.  The trailing 'X's are
;; replaced with a unique alphanumeric combination.

(set '_mktemp (import library "mktemp"))

(define (mktemp template)
	(zero? (_mktemp (string template)))
)

;; @syntax (unix:syslog <num-priority> <str-message> ...)
;; @param <num-priority> The priority of the message.
;; @param <str-message> The message string and template in printf format.
;; @return Returns 'true' or 'nil' depending on success
;;
;; The function writes message to the system message logger.  The
;; message is then written to the system console, log files, logged-in
;; users, or forwarded to other machines as appropriate.

(set '_syslog (import library "syslog"))

(define (syslog priority message)
	(zero? (_syslog (int priority 0) (string message)))
)

; eof

