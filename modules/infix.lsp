;; @module infix.lsp
;; @description Infix expression parser
;; @version 2.1 - comments redone for automatic documentation
;; @version 2.2 - fixed bug for trailing lower priority ops
;; @version 2.3 - doc changes
;; @version 2.3 - doc changes
;; @author Lutz Mueller 2006-2015
;; <h2>Infix expression parser</h2>
;; Parses infix, prefix or postfix expressions given in strings and returns a
;; newLISP expressions, which can be evaluated; captures syntax errors.
;;
;; At the beginning od the program using this module include the following
;; statement:
;; <pre>
;; (load "/usr/local/share/newlisp/modules/infix.lsp")
;; ; or shorter
;; (module "infix.lsp")
;; </pre>

;; @syntax (INFIX:xlate <str-expression> [<context-target>])
;; @param <str-expression> The infix expression in a string
;; @param <context-target> An optional context as compile taret.
;; @return A newLISP expression or 'nil' on failure.
;; When 'nil' is returned then the error message is in 'result'.
;; As an optional second parameter a target context can be passed, 
;; if not used, MAIN is assumed.
;;
;; Note that the parser requires operators, variables and constants surrounded
;; by spaces except where parenthesis are used.
;;        
;; @example
;; (INFIX:xlate "3 + 4") => (add 3 4) ;; parses infix
;; (INFIX:xlate "+ 3 4") => (add 3 4) ;; parses prefix s-expressions
;; (INFIX:xlate "3 4 +") => (add 2 4) ;; parses postfix
;;
;; (INFIX:xlate "3 + * 4") => "ERR: missing argument for +"
;;
;; (eval (INFIX:xlate "3 + 4")) => 7
;;
;; (INFIX:xlate "(3 + 4) * (5 - 2)")  => (mul (add 3 4) (sub 5 2))
;;
;; (INFIX:xlate "(a + b) ^ 2 + (a - b) ^ 2") => (add (pow (add a b) 2) (pow (sub a b) 2))
;;
;; (INFIX:xlate "x = (3 + sin(20)) * (5 - 2)")  => (setq x (mul (add 3 (sin 20)) (sub 5 2)))
;;
;; (INFIX:xlate "x = (3 + sin(10 - 2)) * (5 - 2)")  
;;         => (setq x (mul (add 3 (sin (sub 10 2))) (sub 5 2)))

; operator priority table
; (token operator arg-count priority)
;
(context 'INFIX)

(set 'operators '(
  ("=" setq 2 2) 
  ("+" add 2 3) 
  ("-" sub 2 3) 
  ("*" mul 2 4) 
  ("/" div 2 4)
  ("^" pow 2 5)
  ("abs" abs 1 9)
  ("acos" acos 1 9)
  ("asin" asin 1 9)
  ("atan" atan 1 9)
  ("sin" sin 1 9)
  ("sqrt" sqrt 1 9)
  ("tan" tan 1 9)
  ("cos" cos 1 9)
; add what else is needed
  ))

(set 'targetContext MAIN)

(define (xlate str ctx)
  (if ctx (set 'targetContext ctx))
  (if (catch (infix-xlate str) 'result)
    result                     ; if starts with ERR: is error else result
    (append "ERR: " result)))  ; newLISP error has ocurred


(define (infix-xlate str)
  (set 'tokens (parse str))
  (set 'varstack '())
  (set 'opstack '())
  (dolist (tkn tokens)
	(case tkn
        ("(" (push tkn opstack))
        (")" (close-parenthesis))
        (true (if (assoc tkn operators)
                  (process-op tkn)
                  (push tkn varstack)))))
  (while (not (empty? opstack))
        (make-expression))

  (set 'result (first varstack))
  (if (or (> (length varstack) 1) (not (list? result)))
    (throw "ERR: wrong syntax")
    result))


; pop all operators and make expressions
; until an open parenthesis is found
;
(define (close-parenthesis)
 (while (not (= (first opstack) "("))
    (make-expression))
 (pop opstack))
  

; pop all operator, which have higher/equal priority
; and make expressions
;
(define (process-op tkn)
  (while (and opstack
              (<= (lookup tkn operators 3) (lookup (first opstack) operators 3)))
        (make-expression))
  (push tkn opstack))

; pops an operator from the opstack and makes/returns an
; newLISP expression
;
(define (make-expression)
  (set 'expression '())
  (if (empty? opstack) 
        (throw "ERR: missing parenthesis"))
  (set 'ops (pop opstack))
  (set 'op (lookup ops operators 1))
  (set 'nops (lookup ops operators 2))
  (dotimes (n nops)
    (if (empty? varstack) (throw (append "ERR: missing argument for " ops)))
    (set 'vars (pop varstack))
    (if (atom? vars)
            (if (not (or (set 'var (float vars))
                         (and (legal? vars) (set 'var (sym vars targetContext))) ))
                (throw (append vars "ERR: is not a variable"))
                (push var expression))
            (push vars expression)))
  (push op expression)
  (push expression varstack))

(context 'MAIN)
    
; eof ;

  
  
