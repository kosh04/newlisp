;; @module unix.lsp
;; @description Interface to various UNIX libc functions
;; @version 0.1 alpha - mostly untested
;; @version 0.2 alpha - eliminated getpid and getppid, included in 'sys-info'
;; @version 0.3 alpha - added diferent libc.so library path for UBUNTU 9.04
;; @version 0.4 alpha - take care of overwrite protection of imports in ffilib versions
;; @version 0.5 alpha - precautions for multiple load of module because of overwritten symbols
;; @version 0.6 alpha - libc library path for Linux UBUNTU
;; @version 0.71 alpha - libc library path for several OS
;; @author L.M. 2006-Sep-12, 2012, 2013 lutz@nuevatec.com
;; 
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
;; 'getuid', 'geteuid', 'getgid', 'getegid', 
;; 'setuid', 'seteuid', 'setgid', 'setegid',
;; 'kill', 'chmod', 'ioctl', 'mkfifo', 'mltemp', 'syslog'

(context 'unix)

(set 'files (list
    "/usr/lib/libc.dylib" ; MacOS/Darwin
    "/usr/lib/libc.so.51.0" ; OpenBSD 4.6
    "/lib/x86_64-linux-gnu/libc.so.6" ; Ubuntu 12.04 LTS
    "/lib/i386-linux-gnu/libc.so.6" ; UBUNTU Linux
    "/lib/i686-linux-gnu/libc.so.6" ; UBUNTU Linux
    "/lib64/libc.so.6" ; CentOS 6.x
    "/lib/libc.so.6" ; UBUNTU Linux 9.04
    "/usr/lib/libc.so" ; Linux, BSD, Solaris
))

(set 'library (files (or
		       (find true (map file? files))
		       (throw-error "cannot find standard C library (libc)"))))


(unless module-is-loaded
    (import library "getuid")
    (import library "geteuid")
    (import library "getgid")
    (import library "getegid")
    (set '_setuid (import library "setuid"))
    (set '_seteuid (import library "seteuid"))
    (set '_setegid (import library "setegid"))
    (set '_setgid (import library "setgid"))
    (set '_kill (import library "kill"))
    (set '_chmod (import library "chmod"))
    (set '_ioctl (import library "ioctl"))
    (set '_mkfifo (import library "mkfifo"))
    (set '_mktemp (import library "mktemp"))
    (set '_syslog (import library "syslog"))
    (set 'module-is-loaded true)
)

;; @syntax (unix:getuid)
;; @return Returns the real and effective user ID

;; @syntax (unix:geteuid)
;; @return Returns the effective user ID

;; @syntax (unix:getgid)
;; @return Returns the real group ID.

;; @syntax (unix:getegid)
;; @return Returns the effective group ID

;; @syntax (unix:setuid <num-id>)
;; @param <num-id> The number of the real and effective user IDs to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Set the real and effective user IDs.
;; If no argument or the argument given is not a number then sets
;; current real and effective user IDs.

(constant 'setuid  (lambda (id)
	(zero? (_setuid (int id (getuid))))
))

;; @syntax (unix:seteuid <num-id>)
;; @param <num-id> The number of the effective user ID to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Sets the effective user ID.
;; If no argument or the argument given is not a number then sets
;; current effective user ID.
;;

(constant 'seteuid (lambda (id)
	(zero? (_seteuid (int id (geteuid))))
))

;; @syntax (unix:setgid <num-id>)
;; @param <num-id> The number of the real and effective group ID to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Sets the real and effective group IDs.
;; If no argument or the argument given is not a number then sets
;; current real and effective group IDs.

(constant 'setgid (lambda (id)
	(zero? (_setgid (int id (getgid))))
))

;; @syntax (unix:setegid <num-id>)
;; @param <num-id> the number of the effective group ID to set.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Sets the effective group ID.
;; If no argument or the argument given is not a number then sets
;; current effective group ID.

(constant 'setegid (lambda (id)
	(zero? (_setegid (int id (getegid))))
))

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

(constant 'kill (lambda (pid sig)
	(zero? (_kill (int pid 0) (int sig 0)))
))

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

(constant 'chmod (lambda (file-path option)
	(zero? (_chmod (string file-path) (int option 0644)))
))

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

(constant 'ioctl (lambda (id request argp)
	(zero? (_ioctl (int id 0) (int request 0) (string argp)))
))

;; @syntax (unix:mkfifo <str-path> <num-mode>)
;; @param <str-path> The path  of the new fifo filename.
;; @param <num-mode> The persmissions mode number.
;; @return Returns 'true' or 'nil' depending on success
;;
;; Creates a new fifo file with name path.  The access permissions
;; are specified by mode and restricted by the 'umask(2)' of the calling
;; process.

(constant 'mkfifo (lambda (file-path mode)
	(zero? (_mkfifo (string file-path) (int mode)))
))

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

(constant 'mktemp (lambda (template)
	(zero? (_mktemp (string template)))
))

;; @syntax (unix:syslog <num-priority> <str-message> ...)
;; @param <num-priority> The priority of the message.
;; @param <str-message> The message string and template in printf format.
;; @return Returns 'true' or 'nil' depending on success
;;
;; The function writes message to the system message logger.  The
;; message is then written to the system console, log files, logged-in
;; users, or forwarded to other machines as appropriate.

(constant 'syslog (lambda (priority message)
	(zero? (_syslog (int priority 0) (string message)))
))

; eof

