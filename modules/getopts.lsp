;; @module getopts.lsp
;; @description Parse short and long command line options according to POSIX rules
;; @version 1.0 initial release
;; @author Ted Walther, July 2011
;;
;; POSIX options come in 4 types; long and short, with or without an extra argument.
;;
;; Short options are just a single letter with a <tt>-</tt> in front. '-a -v -h' are single options.  Short options can be collapsed together.  '-a -v -h' is the same thing as '-avh'
;;
;; A short option that takes an argument can take the following two forms: '-fmyletter.txt' is the same as '-f myletter.txt'
;;
;; Long options start with '--'.  '--quiet' and '--help' are good examples.
;;
;; A long option that takes an argument can be in the following two forms: '--file=myletter.txt' is the same as '--file myletter.txt'
;;
;; Here is an example of how to use this module.  It includes a bunch of standard options that every GNU program should support.
;; 
;; @example
;;
;; (module "getopts.lsp")
;; 
;; (setq version-string "Version: 1.0 (2011)")
;;
;; (shortopt "V" (getopts:die version-string) nil "Print version string")
;; (shortopt "v" (++ verbosity) nil "Increase verbosity")
;; (shortopt "q" (setq verbosity 0) nil "Quiet")
;; (shortopt "?" (getopts:usage) nil "Print this help message")
;; (shortopt "h" (getopts:usage) nil "Print this help message")
;; (shortopt "o" (setq output-file getopts:arg) "file" "Output file")
;; (longopt "help" (getopts:usage) nil "Print this help message")
;; (longopt "quiet" (setq verbosity 0) nil "Quiet")
;; (longopt "verbose" (++ verbosity) nil)
;; (longopt "version" (getopts:die version-string) nil "Print version string")
;; (longopt "output" (setq output-file getopts:arg) "file" "Output file")
;; 
;; (println (main-args))
;; (println (getopts (2 (main-args))))
;; (println "Verbosity: " verbosity)
;; (println "Output To: " output-file)
;; (exit)

;; @syntax (shortopt <opt> <action> <arg?> <desc>)
;; @param <opt> The single letter option
;; @param <action> The code to execute when the option is found
;; @param <arg?> 'nil' if the option doesn't take an argument, otherwise a string that describes the type of argument the option takes.
;; @param <desc> A string that describes the option.  This is used by the 'usage' function.
;; @example
;; (shortopt "o" (setq output-file getopts:arg) "file" "Output file")
;; ...
;; $ ./myscript.lsp -ofoo.txt -q
;; $ ./myscript.lsp -qofoo.txt
;; $ ./myscript.lsp -o foo.txt -q
;; $ ./myscript.lsp -qo foo.txt
(define-macro (shortopt opt action arg? desc)
  (if (assoc opt getopts:short)
      (setf (assoc opt getopts:short) (list opt (list action arg? (or desc ""))))
    (push (list opt (list action arg? (or desc ""))) getopts:short)))

;; @syntax (longopt <opt> <action> <arg?> <desc>)
;; @param <opt> The long option
;; @param <action> The code to execute when the option is found
;; @param <arg?> 'nil' if the option doesn't take an argument, otherwise a string that describes the type of argument the option takes.
;; @param <desc> A string that describes the option.  This is used by the 'usage' function.
;; @example
;; (longopt "output" (setq output-file getopts:arg) "file" "Output file")
;; ...
;; $ ./myscript.lsp --output=foo.txt --quiet
;; $ ./myscript.lsp --verbose --output foo.txt
(define-macro (longopt opt action arg? desc)
  (if (assoc opt getopts:long)
      (setf (assoc opt getopts:long) (list opt (list action arg? (or desc ""))))
    (push (list opt (list action arg? (or desc ""))) getopts:long)))

;; @syntax getopts:arg
;;
;; The variable <getopts:arg> holds the argument to the option, for those options
;; which take an argument.  This is useful inside the <action> code, so you can make
;; use of the argument.  For instance, '--prefix=/usr/bin' on the command line, would
;; leave the value '/usr/bin' in <getopts:arg>, and your <action> code could store the
;; value or act on it in some other way.

(context 'getopts)

(define short (list))
(define long (list))

;; @syntax (getopts:usage)
;; @return Exits script with a value of 0
;; Prints out every command line option that has been registered with the getopts module.
;;
;; @example
;; (shortopt "?" (getopts:usage) nil "Print this help message")
;; ...
;; $ ./myscript.lsp -?
;; ==>
;; Usage: ./myscript.lsp [options]
;; 	 -o file           	Output file
;; 	 -h                	Print this help message
;; 	 -?                	Print this help message
;; 	 -q                	Quiet
;; 	 -v                	Increase verbosity
;; 	--output file           	Output file
;; 	--verbose                	
;; 	--quiet                	Quiet
;; 	--help                	Print this help message
;;

(define (usage)
  (println "Usage: " (main-args 1) " [options]")
  (dolist (o short) (println (format "\t -%s %-15s\t%s" (o 0) (or (o 1 1) "") (o 1 2))))
  (dolist (o long) (println (format "\t--%s %-15s\t%s" (o 0) (or (o 1 1) "") (o 1 2))))
  (exit 0))

;; @syntax (getopts:die <format-string> [<format options...>])
;; @return Exits script with a value of 1
;; Prints an error message, then exits.  Syntax is exactly the same as the 'format' function.

(define-macro (die)
  (println (eval (append (list 'format) (args)))) (exit 1))

(define (getopts:getopts arglst)
  (let (unoption-args (list))
    (while arglst
      (let (a (pop arglst))
	(cond
	 ((regex "^--$" a)
	  (setq unoption-args (append unoption-args arglst) arglst nil))
	 ((regex "^--([^=]+)=(.*)$" a)
	  (let (opt $1 arg $2)
	    (unless (lookup opt long)
	      (die {Unrecognized option "%s" {%s}} opt a))
	    (unless ((lookup opt long) 1)
	      (die {Option "%s" doesn't take an argument! {%s}} opt a))
	    (when (empty? arg)
	      (die {No argument supplied for option "%s" {%s}} opt a))
	    (eval (first (lookup opt long)))))
	 ((regex "^--([^=]+)$" a)
	  (let (opt $1 arg nil)
	    (unless (lookup opt long)
	      (die {Unrecognized option "%s" {%s}} opt a))
	    (when ((lookup opt long) 1)
	      (unless arglst 
		(die {No argument supplied for option "%s" {%s}} opt a))
	      (setq arg (pop arglst)))
	    (eval (first (lookup opt long)))))
	 ((regex "^-([^-]+)$" a)
	  (let (options $1)
	    (while (not (empty? options))
	      (let (opt (pop options) arg nil)
		(unless (lookup opt short)
		  (die {Unrecognized option "%s" {%s}} opt a))
		(when ((lookup opt short) 1)
		  (if (not (empty? options))
		      (setq arg options options "")
		    (if arglst
			(setq arg (pop arglst))
		      (die {No argument supplied for option "%s" {%s}} opt a))))
		(eval (first (lookup opt short)))))))
	 (true (push a unoption-args -1))
	 )
	)
      )
    unoption-args
    )
  )

(context MAIN)

;; @syntax (getopts <arglist>)
;; @param <arglist> A list of strings, typically a subset of (main-args)
;; @return The list of all command line arguments that were NOT options.
;; After you have set up all the options using 'shortopt' and 'longopt', call 'getopts' to parse the commandline.


