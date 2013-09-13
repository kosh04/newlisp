;; @module macro.lsp
;; @description Rewrite macros for newLISP
;; @version 1.03
;; @version 1.04 typo in documentation
;; @version 1.1 make it work with default functors
;; @author Lutz Mueller 2010
;; <h2>Introduction</h2>
;; This module implements rewrite macros. Rewrite macros take an expression
;; and rewrite it to a different expression:
;; <pre>
;; (macro (double A) (mul 2 A) ) ; doubles any number in A
;; (macro (queue A L) (pop (push A L -1)) ) ; models a queue<br>
;; (set 'x 3)
;; (double x) => 6
;; (double (add x 2)) => 10<br>
;; (set 'lst '(1 2 3))
;; (queue 'x lst) => 1
;; lst => (2 3 x)</pre>
;; Whenever newLISP reads a '(double A)' expression it will translate
;; it to '(mul 2 A)', expanding A to its content. 
;; When specifying macros using the 'macro' function, variable names must start with
;; uppercase. Variables are optional. The rewrite can be a complex, nested functional 
;; expression like shown in the 'queue' rewrite.
;;
;; The module should be loaded using either:
;; <pre> (module "macro.lsp")</pre>
;; which loads from the 'NEWLISPDIR/modules' standard path, or using:
;; <pre> (load "/mypath/macro.lsp")</pre>
;; Macro translation will occur whenever newLISP parses source, either
;; caused by a 'load' statement or when using 'eval-string'.
;;
;; Macros can be used accross contexts as expected. The following will work:
;; <pre>
;; (module "macro.lsp")<br>
;; (context 'FOO)
;; (macro (sumsq X Y) (add (pow X 2) (pow Y 2)) )
;; (context MAIN)<br>
;; (FOO:sumsq 3 4) => 25
;; </pre>
;; Rewrite macros are faster than <i>fexpr's</i> written with <tt>define-macro</tt>, 
;; because they lack the function call overhead. They also improve readability, 
;; making source code appear shorter. Many functions too small to waste lambda
;; function call overhead can be written as efficiently executing macros.
;;
;; Rewrite macros are cannot be used with <tt>map</tt> or <tt>apply</tt>, they are 
;; not functions.
;;
;; <h2>Internals</h2>
;; This module uses the 'reader-event' function to intercept newLISP's expression 
;; reading and translating process and pre-translates expressions before returning 
;; them to newLISP's evaluation routines. 
;;
;; The function 'macro' creates a 'lambda-macro' function, which is used to
;; to create the expansion of the original call pattern. During the rewrite
;; process this expansion function is used to gerenerate a replacemt for the 
;; original call pattern.
;;
;; This version of <tt>macro.lsp</tt> does not allow rewrite of atomic expressions
;; or rewriting to atomic expressions. Both parts, the original and the rewrite
;; must be functional list expressions. This version allows the usage of macros
;; calls inside macros as long as the macros used have been registered previously.
;;
;; Macro translation increases the load-time of a program or script.
 

(context 'macro)

(if (not macro-list) (setq macro-list '()))

;; @syntax (macro <list-expr> <list-rewrite>)
;; @param <list-expr> The list expression od the call pattern.
;; @param <list-rewrite> The rewritten call pattern.
;; @return A function used by the modules 'rewrite' routine.
;; The function registers a macro. During source translation the call pattern
;; in <list-expr> is re-written to the expansion in <list-rewrite>.
;; Local variables in <list-expr> and <list-rewrite> must be in uppercase:
;;
;; @example
;; (module "macro.lsp")
;;
;; (macro (square A) (mul A A) ) ; squares any number in A
;; (macro (queue A L) (pop (push A L -1)) ) ; models a queue

(define-macro (macro:macro callp body)
	; make sure patterns and replacements are lists, and macro is new
	(unless (and (list? callp) (list? body) (not (eval (callp 0))))
		(throw-error "Wrong arguments or double definition."))
    ; if functor is default functor push context symbol
    (if (= (term (prefix (callp 0))) (term (callp 0)))
	    (push (list (prefix (first callp)) '*) macro-list -1)
	    (push (list (first callp) '*) macro-list -1))
	(eval (expand '(define-macro callp (expand 'body)) 'callp 'body))
)

; this is the rewrite function accepting an expression
; from newLISP and returning a transformation into another
; expression.
(define (rewrite expr)
	(if (list? expr) 
		(dolist (pattern macro-list)
			(if (match pattern expr)  
				(setf expr (eval expr))
				(set-ref-all pattern expr (eval $it) match)) )
	)
	expr
)

;; @syntax (macro:delete <sym-macro>)
;; @param <sym-macro> The unquoted symbol of the macro to delete.
;; <br>Before a macro can be redefined, it must be deleted. Trying
;; do redefine a macro before deleting it will throw an error.
;;
;; @example
;; (macro:delete cube)

(define-macro (macro:delete sname) ; must be prefixed to overwrite built-in
	(pop-assoc sname macro-list)
	(set sname nil) true)

;; @syntax (macro:resume)
;; <br>Resumes the working of all registered macros.

(define (resume)
	(reader-event rewrite))

;; @syntax (macro:suspend)
;; <br>Disables all registered macros. To reenable macros use '(macro:resume)'.

(define (suspend)
	(reader-event 'nil) true)

; hook the rewrite function into newLISP's internal expression
; reader/translator
(reader-event rewrite)

(context MAIN)

; eof
